rule = {
  matches = {
    {{ 
      "device.name", 
      "matches", 
      "alsa_card.pci-0000_00_1f.3"
    }}
 },
  apply_properties = {
    ["api.alsa.period-size"] = 1024,
  },
}
table.insert(alsa_monitor.rules,rule)