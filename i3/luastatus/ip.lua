widget = {
   plugin = 'network-linux',
   cb = function(t)
      local r = {}
      for iface, params in pairs(t) do
	 local addr = params.ipv4 or params.ipv6 
	 if addr then
	    iface = iface:gsub(':.*', '')
	    addr = addr:gsub('%%.*', '')
	    
	    if iface ~= 'lo' then
	       r[#r + 1] = {
		  full_text = string.format(' %s %s', iface, addr),
		  color = '#709080',
	       }
	    end
	 end
      end
      return r
    end,
}
