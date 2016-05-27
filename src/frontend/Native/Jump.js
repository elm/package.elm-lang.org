var _elm_lang$pkgs$Native_Jump = function()
{

var jump = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	var hash = window.location.hash;
	if (hash.length > 0 && hash[0] === '#')
	{
		var id = hash.slice(1);
		var target = document.getElementById(id);
		if (target === null)
		{
			window.location.hash = "";
		}
		if (typeof target.scrollIntoView === 'function')
		{
			target.scrollIntoView(true);
		}
	}
	return callback(_elm_lang$core$Native_Scheduler.succeed({ ctor: '_Tuple0' }));
});

return {
	jump: jump
};

}();
