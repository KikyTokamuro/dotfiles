widget = luastatus.require_plugin('battery-linux').widget{
   period = 5,
   dev = 'BAT1',
   cb = function(t)
      local symbol = ({
            Charging    = '',
            Discharging = '',
      })[t.status] or ''

      local full_text
      
      if t.rem_time then
	 local h = math.floor(t.rem_time)
	 local m = math.floor(60 * (t.rem_time - h))
	 full_text = string.format('%s %3d%% ~ %02dh %02dm', symbol, t.capacity, h, m)
      else
	 full_text = string.format('%s %3d%%', symbol, t.capacity)
      end

      return {
	 {full_text = full_text, color = '#d68fff'},
	 rem_seg,
      }
    end,
}
