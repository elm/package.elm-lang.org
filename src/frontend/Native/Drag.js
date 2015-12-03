Elm.Native.Drag = {};
Elm.Native.Drag.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Drag = localRuntime.Native.Drag || {};
	if (localRuntime.Native.Drag.values)
	{
		return localRuntime.Native.Drag.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);


	var unit = { ctor: '_Tuple0' };


	function track(move, end)
	{
		return Task.asyncFunction(function(callback)
		{
			function reportMoves(event)
			{
				Task.perform(move(event.pageX));
			}

			function reportEnd()
			{
				window.removeEventListener('mousemove', reportMoves);
				window.removeEventListener('mouseup', reportEnd);
				callback(Task.succeed(end));
			}

			window.addEventListener('mousemove', reportMoves);
			window.addEventListener('mouseup', reportEnd);
		});
	}

	return localRuntime.Native.Drag.values = {
		track: F2(track)
	};
};
