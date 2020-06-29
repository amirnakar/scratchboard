#### Read Kaiser to plots ####

## I want to import all the kaiser data in a folder into a big df.

path = "C:/Users/di58lag/Desktop/PhD/Projects/IRAgar/Data/200604 - Converted/4AAP"
list.of.files = dir(path = path, full.names = T, recursive = T)
read.csv(list.of.files[1])
list.of.files

spectra = read.csv(list.of.files[1])
plot(spectra)

wavenumbers = read.csv(list.of.files[1])[,1]
wavenumbers

spec.df = data.frame(wavenumbers)

for i = 1:length(list.of.files){
  spectra = read.csv(list.of.files[i])[,2]
  spec.df = cbind(spec.df, spectra)
}
i
