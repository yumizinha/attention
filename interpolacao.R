interpolate_signal = function(signal, these_bad_channels)
{
  for(channel in these_bad_channels)
  {
    neighbors = which(interpol[channel,] == 1)
    neighbors = neighbors[!(neighbors %in% these_bad_channels)]
    if(length(neighbors > 1))
    {
      signal[,channel] = rowMeans(as.matrix(signal[,neighbors]), na.rm = TRUE)
      print(paste("canal", channel, ": interpolados"))
    }
    else
    {
      signal[,channel] = rowMeans(signal, na.rm = TRUE)
      print(paste("canal", channel, ": sem vizinhos válidos"))
    }
  }
  signal
}

good_channels = read.csv("Good_channels.csv")[, -1]
interpol = read.csv("interpol.csv")[, -1]
num_patients = length(list.files("./Oxy/"))
patients = 1:num_patients
for(patient in patients) {
  print(paste("ID: ", patient))
  these_bad_channels = which(!good_channels[patient,])
  
  oxy_path = paste("./Oxy/", patient, ".txt", sep = "")
  signal = interpolate_signal(read.table(oxy_path), these_bad_channels)
  oxy_n_path = paste("./Oxy_interpol/", patient, ".txt", sep = "")
  write.table(signal, file = oxy_n_path)
  
  deoxy_path = paste("./Deoxy/", patient, ".txt", sep = "")
  signal = interpolate_signal(read.table(deoxy_path), these_bad_channels)
  deoxy_n_path = paste("./Deoxy_interpol/", patient, ".txt", sep = "")
  write.table(signal, file = deoxy_n_path)
}
