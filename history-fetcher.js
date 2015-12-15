var fs = require('fs');
var path = require('path');
var process = require('process');
var GitHubApi = require("github");


var github = new GitHubApi({
	version: "3.0.0",
	protocol: "https",
	headers: {
		"User-Agent": "get-dates-of-tags-for-elm-packages-which-only-needs-to-happen-once"
	}
});


var username = process.argv[2];
var password = process.argv[3];
if (!username || !password)
{
	console.error(
		'Provide a user name and password to get a rate-limit of 5000 per hour.\n'
		+ 'Without this, the rate-limit is 60 per hour.\n'
	);
}
else
{
	github.authenticate({
		type: "basic",
		username: username,
		password: password
	});
}


function getDirectories(dir)
{
	var root = path.join('packages', dir);
	return fs.readdirSync(root).filter(function(file)
	{
		return fs.statSync(path.join(root, file)).isDirectory();
	});
}


function exists(filePath)
{
	try
	{
		var stats = fs.statSync(filePath);
		return stats.isFile();
	}
	catch (e)
	{
		return false;
	}
}


function prepend(root)
{
	return function(file)
	{
		return path.join(root, file);
	}
}


function flatten(arrays)
{
	return [].concat.apply([], arrays);
}


function contains(x, arrays)
{
	return arrays.indexOf(x) !== -1;
}


function sequence(todos, asyncThing, callback)
{
	return sequenceHelp([], todos, asyncThing, callback);
}


function sequenceHelp(dones, todos, asyncThing, callback)
{
	if (todos.length === 0)
	{
		return callback(dones);
	}

	asyncThing(todos[0], function(result) {
		sequenceHelp(dones.concat([result]), todos.slice(1), asyncThing, callback);
	});
}



// TRAVERSE PACKAGES


function traverse()
{
	var users = getDirectories('.');
	return flatten(users.map(traverseUser));
}


function traverseUser(user)
{
	var repos = getDirectories(user).filter(function(repo) {
		return !exists(path.join('packages', user, repo, 'history.json'))
	});
	return repos.map(traverseRepo(user)).filter(function(project) {
		return project.versions.length > 0;
	});
}


function traverseRepo(user)
{
	return function(repo)
	{
		var versions = getDirectories(path.join(user, repo));
		return {
			user: user,
			repo: repo,
			versions: versions
		};
	};
}



// CREATE HISTORY


function createHistory(project, callback)
{
	var user = project.user;
	var repo = project.repo;
	var versions = project.versions;

	var msg = {
		user: user,
		repo: repo,
		per_page: 100
	};

	github.repos.getTags(msg, function(err, allTagInfo)	{
		var tags = allTagInfo.map(simplifyTag).filter(function(tag) {
			return contains(tag.name, versions);
		});

		checkOneToOne(user, repo, versions, tags);

		sequence(tags, toRelease(user, repo), writeHistory(user, repo, callback));
	});
}


function simplifyTag(fancyTag)
{
	return {
		name: fancyTag.name,
		sha: fancyTag.commit.sha
	};
}


function checkOneToOne(user, project, versions, tags)
{
	var foundVersions = tags.map(function(tag) { return tag.name; });

	var allFound = versions.every(function(version) {
		return contains(version, foundVersions);
	});

	if (allFound && versions.length === versions.length)
	{
		return;
	}

	console.error(
		'Not getting all the versions I need for ' + user + '/' + repo + ':\n'
		+ '  Needed: ' + versions + '\n'
		+ '  Found: ' + foundVersions
	);
}


function toRelease(user, repo)
{
	return function(tag, callback)
	{
		var msg = {
			user: user,
			repo: repo,
			sha: tag.sha
		};
		github.gitdata.getCommit(msg, function(err, info) {
			var date = new Date(info.committer.date);
			callback({
				version: tag.name,
				added: 0,
				changed: 0,
				removed: 0,
				date: date.getTime()
			});
		});
	}
}


function writeHistory(user, repo, callback)
{
	return function(releases)
	{
		var historyPath = path.join("packages", user, repo, "history.json");
		var history = JSON.stringify(releases, null, 4);
		fs.writeFile(historyPath, history, function(err) {
			if (err)
			{
				return console.error(err);
			}

			console.log('History of ' + user + '/' + repo + ' generated.');
			callback(null);
		});
	}
}



// MAIN


var projects = traverse();
sequence(projects, createHistory, function(histories) {
 	console.log('DONE');
});

