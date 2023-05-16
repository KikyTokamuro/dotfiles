widget = luastatus.require_plugin('mem-usage-linux').widget{
   timer_opts = {period = 5},
   cb = function(t)
      local used_kb = t.total.value - t.avail.value
      return {full_text = string.format(' %3.2f GiB', used_kb / 1024 / 1024), color = '#2983f0'}
   end,
}
