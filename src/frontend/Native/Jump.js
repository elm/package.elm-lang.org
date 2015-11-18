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
		window.location = window.location.hash;
		return callback(Task.succeed({ ctor: '_Tuple0' }));
	});

	return localRuntime.Native.Jump.values = {
		jump: jump
	};
};
