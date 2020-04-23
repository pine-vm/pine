# 2020-04-23 Elm-fullstack - Streamline Deployment

A few weeks ago, the first design and implementation for migrations emerged.
In the last five days, I deployed new versions of several apps to production, which leads me to summarize my observations about the process now.

Here is an overview of the process used this week:

+ Staging: Perform live tests combining the app version planned to deploy and a fresh copy of the process state from production.
+ Continued to use the original web host, not the new one adding support for migrations. Given the expected imminent changes in the migrations wrapper, it seemed not worth the effort to migrate existing apps to the new model this time. For the state of the Elm app, migrations were not needed this time because the state model did not change.
+ Analogous to the current state of the guides, build a docker image for the new app program code.
+ Remove the old docker container and start a new one. Continue to use the same docker volume that contains the persistent process state.

I see several areas to improve in this process.

One problem revealed with deployments using that process is zombie-docker containers. A container restarted after we gave a command to stop it. ([Clarification on how to stop containers with --restart=always](https://github.com/moby/moby/issues/10032)) Using the new migrations API will avoid this problem because we don't need a new docker container anymore to deploy a new version of the app.

The costs of preparing live tests can be reduced. This time, I started a new FTP server using the same docker volume to copy the files from production to the test environment. This approach worked OK but can be made more efficient. Often we test only with the latest program state and don't need the whole process history. Having a tool to copy only the parts required here would reduce the amount of data to transfer to a small fraction.

How would the deployment work, ideally? To come up with a streamlined process, I also look at the examples of how automated tests use migrations. At least in one case, the migration is modeled in the same directory as the productive app, using the same `elm.json` file. Since all the info seems to be in place already, why not use a single command to deploy to production based on the current directory (containing the `elm-fullstack.json` file):
```cmd
elm-fullstack deploy
```

We might want to add the address of the target machine explicitly, or there could be a default configured in the project files. That command-line interface would take care of mapping the state found in the current directory to the HTTP API and reporting the result back to the terminal. The deployment can fail in various ways, and we want specific error messages for a pleasant overall experience. Maybe it makes sense to read git status and fail if there are any uncommitted changes. Taking this further, the files to use could be taken out of the last git commit instead of the local file system. Also, to stay organized, we probably want to commit a file that reports the deployment (or the failed attempt) into the project repository. The CLI could produce this file, including the reference to the git commit used as the source for the deployed version. The report should also contain the ID of the process state right before the migration, in case we want to revert the production process state later.

Thinking again about taking the files from git instead of the file system, I find another reason to go this route: Files which are filtered out by .gitignore will not trigger the "uncommitted changes" error described above. Thus it would be more consistent to ensure all gitignored also does not end up in the state used for the deployment. Probably reading the files from the git repository is less effort than emulating the gitignore rules.

In some cases, we want to know if any problems in the current app code would prevent a migration, so there should be a command dedicated to only test deployment:

```cmd
elm-fullstack test deploy
```

Especially in the fast-paced earlier phases of online-game development, there is a higher risk of deploying a flawed version of the backend accidentally. Bugs in the backend implementation can also lead to defects in the database state. There are different ways to fix such damages, but at least there should be a way for a simple reversion to an earlier state. We can achieve such a reversion using backup and restore functionality on the level of the host/VM or storage container. For a DigitalOcean droplet, this process recently took several minutes. With dedicated functionality in the framework, we could reduce this duration a lot. How would the interface look like for such a feature? Maybe copy from [git reset](https://git-scm.com/docs/git-reset)? Because of the implications for operation, a reversion to an earlier state should probably be modeled somehow in the process history, and the CLI should produce a report file that we can add to the project repository.
