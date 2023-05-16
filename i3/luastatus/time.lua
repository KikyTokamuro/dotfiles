widget = {
   plugin = 'timer',
   cb = function()
      return {
	 {full_text = os.date(" %d.%m.%Y   %H:%M")},
      }
   end,
}
