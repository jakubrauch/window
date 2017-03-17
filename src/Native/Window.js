var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

var offset = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
    callback(_elm_lang$core$Native_Scheduler.succeed({
        x: window.pageXOffset || document.documentElement.scrollLeft,
        y: window.pageYOffset || document.documentElement.scrollTop
    }));
});

return {
	size: size,
	offset: offset
};

}();