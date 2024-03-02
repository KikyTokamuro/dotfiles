widget = luastatus.require_plugin('cpu-usage-linux').widget{
   cb = function(usage)
      if usage ~= nil then
	 return {
	    full_text = string.format(' %.1f%%', usage * 100),
	    color = '#eab93d'
	 }
      end
   end,
}
