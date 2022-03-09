#!/usr/bin/Rscript
# Sorry this isn't elegant but necessary for the cron tab to work
setwd("~/Projects/Graphics/2022-log-scales-prolific/perception-of-statistical-graphics/")

# Set up authentication via ssh
cred <- git2r::cred_ssh_key("~/.ssh/id_rsa.pub", "~/.ssh/id_rsa")
repo <- git2r::repository()
git2r::config(repo = repo, global = F, "Susan-auto", "srvanderplas@gmail.com")

# Log job start
httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023/start")

# Check repo status
status <- git2r::status()

tmp <- status$unstaged
modified <- names(tmp) == "modified"
modified <- unlist(tmp[modified])

# If db has been modified
if ("databases/00_demographics_db.db" %in% modified |
    "databases/01_lineups_db.db" %in% modified |
    "databases/02_you_draw_it_db.db" %in% modified |
    "databases/03_estimation_db.db" %in% modified) {

  # Add changed db to commit and commit
  git2r::add(repo = '..', list.files("databases", "*.db$", full.names = T))
  try(git2r::commit(message = "Update data"))

  # Update
  git2r::pull(repo = repo, credentials = cred)
  git2r::push(getwd(), credentials = cred)

  if (length(git2r::status()$unstaged$conflicted) > 0) {
    # Log merge conflict, signal failure (Susan gets an email)
    httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023/fail", body = "Merge conflict")
  } else {
    # Log success
    httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023", body = "Changes pushed")
  }
} else {
  # Log no changes
  httr::POST("https://hc-ping.com/00fb59c2-a334-44ad-899a-1927e3d18023", body = "No changes")
}

git2r::config(repo = repo, global = F, "Susan Vanderplas", "srvanderplas@gmail.com")
