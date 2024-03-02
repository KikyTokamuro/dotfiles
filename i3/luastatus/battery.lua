notify = true

widget = luastatus.require_plugin('battery-linux').widget {
    period = 3 * 60,
    dev = 'BAT1',
    cb = function(t)
        if t.capacity == nil then
	    return nil
        end

        local symbol = ({
            Charging = '',
            Discharging = ''
        })[t.status] or ''

        local full_text

        if t.rem_time then
            local h = math.floor(t.rem_time)
            local m = math.floor(60 * (t.rem_time - h))
            full_text = string.format('%s %3d%% ~ %02dh %02dm', symbol, t.capacity, h, m)
        else
            full_text = string.format('%s %3d%%', symbol, t.capacity)
        end

	if notify and t.capacity <= 25 and t.status == "Discharging" then
	   os.execute("notify-send -u critical 'Battery Low' 'Please connect your charger.'")
	   notify = false
	end

	if t.status == "Charging" then
	   notify = true
	end
	
        return {{
            full_text = full_text,
            color = '#d68fff'
        }, rem_seg}
    end
}
