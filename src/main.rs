use anyhow::{anyhow, Context, Result};
use chrono::{prelude::*, DateTime};
use futures::try_join;
use regex::{Captures, Regex};
use std::path::PathBuf;
use std::{
    collections::HashMap,
    env, fs,
    fs::File,
    hash::{Hash, Hasher},
    io::prelude::*,
    path::Path,
};
use structopt::StructOpt;
use url::Url;

const MELPA_URL: &'static str = "https://melpa.org";
const GITHUB_GRAPHQL_ENDPOINT: &'static str = "https://api.github.com/graphql";
const GITHUB_REPOSITORIES_GRAPHQL_QUERY_FIELDS: &str = "{\
      createdAt, \
      pushedAt, \
      forkCount, \
      licenseInfo {name}, \
      stargazers {totalCount}, \
      issues(filterBy: { states: [OPEN] }) {totalCount}, \
      pullRequests(states: [OPEN]) {totalCount}, \
      releases {totalCount}, \
      vulnerabilityAlerts {totalCount}}";

type Count = u32;
type PackageName = String;
type MelpaDownloadCounts = HashMap<PackageName, Count>;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct MelpaRecipe {
    repo: Option<String>,
    url: Option<Url>,
    fetcher: String,
}

fn build_repository_provider_url(
    provider_base_url: &str,
    namespaced_repository: &Option<String>,
) -> Result<Url> {
    if let Some(repo) = namespaced_repository {
        let url = format!("{}/{}", provider_base_url, repo);
        Url::parse(&url).context(format!("failed to parse {} as an URL", url))
    } else {
        Err(anyhow!(
            "couldn't fallback to empty 'repo' field to build \
             MELPA recipe URL"
        ))
    }
}

fn repository_url_or_error(repository_url: &Option<Url>) -> Result<Url> {
    repository_url.clone().ok_or(anyhow!(
        "couldn't fallback to empty 'url' field to build recipe URL"
    ))
}

impl MelpaRecipe {
    fn url(&self) -> Result<Url> {
        match self.fetcher.as_str() {
            "github" => {
                build_repository_provider_url("https://github.com", &self.repo)
            }
            "gitlab" => {
                build_repository_provider_url("https://gitlab.com", &self.repo)
            }
            "git" => repository_url_or_error(&self.url),
            "hg" => repository_url_or_error(&self.url),
            fetcher => {
                Err(anyhow!("don't know how to build URL for {}", fetcher))
            }
        }
    }
}

fn is_repository_provider_url(url: &&Url) -> bool {
    Regex::new(r"(?:(?:github|gitlab)\.com|framagit\.org)")
        .unwrap()
        .is_match(url.host_str().unwrap())
}

fn extract_repository_owner_and_name<'a>(url: &'a Url) -> Option<Captures<'a>> {
    Regex::new(r"/?(?P<owner>.+?)/(?P<name>[^/]+)(?:\.git)?")
        .unwrap()
        .captures(url.path())
}

type MelpaRecipes = HashMap<PackageName, MelpaRecipe>;

fn _configuration_file_path(home_directory: &str, file_name: &str) -> String {
    format!(
        "{}/.config/{}/{}",
        home_directory,
        build_info::PKG_NAME,
        file_name
    )
}

fn _user_data_file_path(home_directory: &str, file_name: &str) -> String {
    format!(
        "{}/.local/share/{}/{}",
        home_directory,
        build_info::PKG_NAME,
        file_name
    )
}

fn cache_file_path(home_directory: &String, file_name: &str) -> String {
    format!(
        "{}/.cache/{}/{}",
        home_directory,
        build_info::PKG_NAME,
        file_name
    )
}

fn write_json_file(file_path: &str, content: &String) -> Result<()> {
    Path::new(&file_path)
        .parent()
        .and_then(|p| fs::create_dir_all(p).ok());

    Ok(File::create(file_path.clone())
        .context(format!("failed to open file {}", &file_path))?
        .write_all(content.as_bytes())
        .context(format!("failed to write file {}", &file_path))?)
}

async fn fetch_melpa_recipes() -> Result<MelpaRecipes> {
    let home_directory =
        env::var("HOME").context("HOME environment variable is required")?;

    let response: reqwest::Response =
        reqwest::get(&format!("{}/recipes.json", MELPA_URL))
            .await
            .context("failed to fetch MELPA recipes")?;

    let response_headers = response.headers().clone();
    let _last_modified = response_headers.get("last-modified");
    let _content_length = response_headers.get("content-length");
    let response_text = response
        .text()
        .await
        .context("failed to extract text from MELPA response")?;
    let melpa_recipes: MelpaRecipes = serde_json::from_str(&*response_text)
        .context("failed to deserialize MELPA response into JSON")?;

    // eprintln!("{:#?}", content_length);
    // eprintln!("{:#?}", response_headers);
    // eprintln!("{:#?}", last_modified);

    write_json_file(
        &cache_file_path(&home_directory, "melpa_recipes.json"),
        &serde_json::to_string_pretty(&melpa_recipes)
            .context("failed to pretty print MELPA recipes JSON")?,
    )
    .context("failed to write MELPA recipes file")?;

    Ok(melpa_recipes)
}

async fn fetch_melpa_download_counts() -> Result<MelpaDownloadCounts> {
    let home_directory =
        env::var("HOME").context("HOME environment variable is required")?;

    let response: reqwest::Response =
        reqwest::get(&format!("{}/download_counts.json", MELPA_URL))
            .await
            .context("failed to fetch MELPA download counts")?;

    let response_headers = response.headers().clone();
    let _last_modified = response_headers.get("last-modified");
    let _content_length = response_headers.get("content-length");
    let response_text = response
        .text()
        .await
        .context("failed to extract text from MELPA response")?;
    let melpa_download_counts: MelpaDownloadCounts =
        serde_json::from_str(&*response_text)
            .context("failed to deserialize MELPA response into JSON")?;

    // eprintln!("{:#?}", content_length);
    // eprintln!("{:#?}", response_headers);
    // eprintln!("{:#?}", last_modified);

    write_json_file(
        &cache_file_path(&home_directory, "melpa_download_counts.json"),
        &response_text,
    )
    .context("failed to write MELPA download counts file")?;

    Ok(melpa_download_counts)
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct Repository {
    url: Url,
    owner: String,
    name: String,
}

impl From<MelpaRecipe> for Option<Repository> {
    fn from(recipe: MelpaRecipe) -> Self {
        let url = recipe.url().ok()?;
        let repo = recipe.repo?;
        let parts: Vec<&str> = repo.splitn(2, r"/").collect();
        if let (Some(owner), Some(name)) = (parts.get(0), parts.get(1)) {
            Some(Repository {
                url,
                owner: owner.to_string(),
                name: name.to_string(),
            })
        } else {
            None
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct Package {
    // from package.el.
    name: PackageName,
    summary: String,
    version: Vec<i64>,
    dir: String,
    maintainer: String,
    license: Option<String>, // fallbacks to elx.el.
    kind: Option<String>,
    signed: Option<bool>,
    archive: Option<String>,
    url: Option<Url>,
    keywords: Option<Vec<String>>,
    authors: Option<Vec<String>>,
    // inferred from the `url` field.
    repository: Option<Repository>,
    // from Melpa.
    melpa_downloads_count: Option<Count>,
    // from Github.
    github_created_at: Option<String>,
    github_pushed_at: Option<String>,
    github_stars_count: Option<Count>,
    github_forks_count: Option<Count>,
    github_issues_count: Option<Count>,
    github_pull_requests_count: Option<Count>,
    github_releases_count: Option<Count>,
    github_vulnerability_alerts_count: Option<Count>,
    github_license: Option<String>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct PackageId(String);

impl PackageId {
    fn safe_identifier(s: &String) -> String {
        Regex::new(r"[^A-Za-z_]")
            .unwrap()
            .replace_all(&s.replace("-", "_"), "")
            .to_string()
    }
}

impl PartialEq for PackageId {
    fn eq(&self, other: &Self) -> bool {
        Self::safe_identifier(&self.0) == Self::safe_identifier(&other.0)
    }
}

impl Eq for PackageId {}

impl Hash for PackageId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::safe_identifier(&self.0).hash(state);
    }
}

type PackageIndex = HashMap<PackageId, Package>;

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct GithubRepositoryLicenseInfo {
    name: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct GithubRepositoryFieldWithCount {
    total_count: Count,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct GithubRepository {
    created_at: DateTime<Utc>,
    pushed_at: DateTime<Utc>,
    fork_count: Count,
    stargazers: GithubRepositoryFieldWithCount,
    issues: GithubRepositoryFieldWithCount,
    pull_requests: GithubRepositoryFieldWithCount,
    releases: GithubRepositoryFieldWithCount,
    vulnerability_alerts: GithubRepositoryFieldWithCount,
    license_info: Option<GithubRepositoryLicenseInfo>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct GithubRepositoryErrorLocation {
    column: i32,
    line: i32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct GithubRepositoryError {
    locations: Vec<GithubRepositoryErrorLocation>,
    message: String,
    #[serde(rename = "type")]
    kind: Option<String>,
    extensions: Option<HashMap<String, String>>,
    path: Vec<String>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct GithubRepositoriesResponse {
    data: Option<HashMap<PackageId, Option<GithubRepository>>>,
    errors: Option<Vec<GithubRepositoryError>>,
}

fn build_repository_query(
    package_id: &PackageId,
    repository: &Repository,
    fields: &str,
) -> String {
    format!(
        "{}: repository(owner: \"{}\", name: \"{}\") {}",
        PackageId::safe_identifier(&package_id.0),
        repository.owner,
        repository.name,
        fields
    )
}

fn parse_repository_from_package_url(
    package: &Package,
) -> Option<Option<Repository>> {
    let repository_url = package.url.clone();

    package
        .url
        .as_ref()
        .filter(is_repository_provider_url)
        .map(extract_repository_owner_and_name)
        .map(|captures| {
            captures.map(|c| Repository {
                owner: c["owner"].to_owned(),
                name: c["name"].to_owned(),
                // It's ok to unwrap here because the regular expression
                // captures will only exist when the `package.url` also
                // exists.
                url: repository_url.unwrap(),
            })
        })
}

type GraphQlRequestBody<'a> = HashMap<&'a str, String>;

fn build_github_repositories_query_request_body<'a>(
    package_index: &PackageIndex,
) -> GraphQlRequestBody<'a> {
    let mut repository_queries: Vec<String> = vec![];

    for (package_id, package) in package_index.iter() {
        if let Some(repository) = package.repository.as_ref() {
            repository_queries.push(build_repository_query(
                package_id,
                repository,
                GITHUB_REPOSITORIES_GRAPHQL_QUERY_FIELDS,
            ));
        }
    }

    let mut request_body = HashMap::new();

    request_body.insert(
        "query",
        format!("query {{{}}}", repository_queries.join(" ")),
    );

    request_body
}

async fn fetch_github_repositories<'a>(
    request_body: GraphQlRequestBody<'a>,
    github_access_token: Option<String>,
    user_agent: &str,
) -> Result<GithubRepositoriesResponse> {
    let home_directory =
        env::var("HOME").context("HOME environment variable is required")?;

    let request = reqwest::Client::new()
        .post(GITHUB_GRAPHQL_ENDPOINT)
        .header("User-Agent", user_agent)
        .header(
            "Authorization",
            format!(
                "bearer {}",
                github_access_token.unwrap_or(
                    env!("EMACS_AUDIT_GITHUB_ACCESS_TOKEN").to_owned()
                )
            ),
        )
        .json(&request_body);
    // eprintln!("{:#?}", request);

    let response = request
        .send()
        .await
        .context("failed to send request to fetch GitHub repositories")?;
    // eprintln!("{:#?}", response.headers());

    let response_body_text = response
        .error_for_status()
        .context(
            "error response for GitHub repositories request. It's \
             likely that the GitHub token expired or is invalid",
        )?
        .text()
        .await
        .context("failed to parse text from GitHub repositories response")?;
    // eprintln!("{:#?}", response_body_text);

    let github_repositories: GithubRepositoriesResponse = serde_json::from_str(
        &response_body_text,
    )
    .context("failed to parse GitHub repositories response into JSON")?;

    write_json_file(
        &cache_file_path(&home_directory, "github_repositories.json"),
        &serde_json::to_string_pretty(&github_repositories)
            .context("failed to pretty print GitHub repositories JSON")?,
    )
    .context("failed to write GitHub repositories file")?;

    Ok(github_repositories)
}

pub mod build_info {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

fn version() -> String {
    format!("{} {}", build_info::PKG_NAME, build_info::PKG_VERSION)
}

fn long_version() -> String {
    let built_at = DateTime::parse_from_rfc2822(build_info::BUILT_TIME_UTC);
    format!(
        "{} ({}{} {})\n{} ({})\n{}",
        version(),
        build_info::GIT_VERSION.expect("failed to get git version"),
        build_info::GIT_DIRTY.map_or("", |is_dirty| if is_dirty {
            "-dirty"
        } else {
            ""
        }),
        built_at.unwrap().format("%Y-%m-%d"),
        build_info::TARGET,
        build_info::PROFILE,
        build_info::RUSTC_VERSION,
    )
}

#[derive(Debug, StructOpt)]
struct EnrichPackageIndex {
    /// Package index JSON file to enrich
    #[structopt(name = "PACKAGE_INDEX_JSON_FILE", parse(from_os_str))]
    file: PathBuf,

    /// GitHub personal access token. Create the token at
    /// https://github.com/settings/tokens and check
    /// https://developer.github.com/v4/guides/forming-calls/#authenticating-with-graphql
    /// for the necessary scopes.
    #[structopt(short, long)]
    token: Option<String>,
}

#[derive(Debug, StructOpt)]
enum Command {
    /// Shows license, Melpa, and GitHub stats for all packages
    EnrichPackageIndex(EnrichPackageIndex),
}

/// Provides facilities for auditing Emacs installations
#[derive(StructOpt, Debug)]
#[structopt(name = "emacs-audit", author)]
struct CliArguments {
    #[structopt(subcommand)]
    subcommand: Option<Command>,

    /// Show verbose version information
    #[structopt(long = "long-version")]
    long_version: bool,
}

async fn enrich_package_index(
    package_index: PathBuf,
    github_access_token: Option<String>,
) -> Result<()> {
    let user_agent = version();

    let package_index_path = package_index
        .to_str()
        .context("error with package index file")?;
    let mut package_index: PackageIndex = serde_json::from_reader(
        File::open(package_index_path)
            .context(format!("failed to open file {}", &package_index_path))?,
    )
    .context(format!("error parsing JSON {}", &package_index_path))?;

    let fetch_melpa_recipes_and_github_repositories = async {
        // `build_github_repositories_query_request_body` needs the
        // `package.repository` field values to be present, so before calling it
        // we download MELPA package recipes (which contain repository
        // information) and fill `Package`s with them if they don't already have
        // a repository.
        //
        // If a MELPA recipe for a given `Package` *also* doesn't have
        // repository information, fallback to extracting repository information
        // from `package.url`.
        let melpa_recipes = fetch_melpa_recipes().await?;

        for (_package_id, package) in package_index.iter_mut() {
            if package.repository.is_none() {
                if let Some(repository) = melpa_recipes
                    .get(&package.name)
                    .map(|recipe| -> Option<Repository> {
                        recipe.clone().into()
                    })
                    .or(parse_repository_from_package_url(package))
                {
                    package.repository = repository;
                }
            }
        }

        let request_body =
            build_github_repositories_query_request_body(&package_index);
        // eprintln!("{:#?}", request_body);

        let github_repositories = fetch_github_repositories(
            request_body,
            github_access_token,
            &user_agent,
        )
        .await?;

        Ok::<(_, _), anyhow::Error>((melpa_recipes, github_repositories))
    };

    let (melpa_download_counts, (melpa_recipes, github_repositories)) = try_join!(
        fetch_melpa_download_counts(),
        fetch_melpa_recipes_and_github_repositories
    )?;

    for (package_id, package) in package_index.iter_mut() {
        if let Some(download_counts) = melpa_download_counts.get(&package.name)
        {
            package.melpa_downloads_count = Some(*download_counts);
        }

        if let Some(recipe) = melpa_recipes.get(&package.name) {
            if package.url.is_none() {
                package.url = recipe.url().ok();
            }
        }

        if let Some(Some(Some(repository))) = github_repositories
            .data
            .as_ref()
            .map(|data| data.get(package_id))
        {
            package.github_stars_count =
                Some(repository.stargazers.total_count);
            package.github_created_at =
                Some(repository.created_at.format("%Y-%m-%d").to_string());
            package.github_pushed_at =
                Some(repository.pushed_at.format("%Y-%m-%d").to_string());
            package.github_forks_count = Some(repository.fork_count);
            package.github_issues_count = Some(repository.issues.total_count);
            package.github_pull_requests_count =
                Some(repository.pull_requests.total_count);
            package.github_releases_count =
                Some(repository.releases.total_count);
            package.github_vulnerability_alerts_count =
                Some(repository.vulnerability_alerts.total_count);
            package.github_license = repository
                .license_info
                .as_ref()
                .map(|license_info| license_info.name.clone())
        }
    }

    println!(
        "{}",
        serde_json::to_string_pretty(&package_index)
            .context("error serializing enriched package index to JSON")?
    );

    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli_arguments = CliArguments::from_args();

    if cli_arguments.long_version {
        println!("{}", long_version());
        return Ok(());
    }

    if let Some(command) = cli_arguments.subcommand {
        match command {
            Command::EnrichPackageIndex(EnrichPackageIndex { file, token }) => {
                return enrich_package_index(file, token).await;
            }
        }
    }

    Ok(())
}
