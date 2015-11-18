Elm.Native.Jump = {};

Elm.Native.Jump.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Jump = localRuntime.Native.Jump || {};
	if (localRuntime.Native.Jump.values)
	{
		return localRuntime.Native.Jump.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);

	var jump = Task.asyncFunction(function(callback) {
		var hash = window.location.hash;
		if (hash.length > 0 && hash[0] === '#')
		{
			var id = hash.slice(1);
			var target = document.getElementById(id);
			if (typeof target.scrollIntoView === 'function')
			{
				target.scrollIntoView(true);
			}
		}
		return callback(Task.succeed({ ctor: '_Tuple0' }));
	});

	return localRuntime.Native.Jump.values = {
		jump: jump
	};
};
