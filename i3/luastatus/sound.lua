widget = {
    plugin = 'alsa',
    cb = function(t)
        local percent = (t.vol.cur - t.vol.min) / (t.vol.max - t.vol.min) * 100

        if t.mute then
            return {
                full_text = string.format('%3d%%', math.floor(0.5 + percent)),
                color = '#e03838'
            }
        else
            local percent = (t.vol.cur - t.vol.min) / (t.vol.max - t.vol.min) * 100
            return {
                full_text = string.format('%3d%%', math.floor(0.5 + percent)),
                color = '#34d21b'
            }
        end
    end
}
