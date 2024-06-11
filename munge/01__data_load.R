#  PFC                                                                   #####
fnames <- list.files("data/raw", pattern="*.csv")

map_df(fnames, \(fname){
  d <- read_csv(file.path("data/raw/", fname), comment = "#")
  session <- str_split(fname, "_")[[1]][3]
  d$session=session
  
  return(d)
}) -> raw_data

## Sanity check:                                                          #####
raw_data |>
  group_by(subj,session,block) |> 
  summarize(nbeep=sum(stimulus=="stimulus"),
            ntap=sum(response %in% c("lctrl", "rctrl")),
            ntapleft=sum(response %in% c("lctrl")),
            ntapright=sum(response %in% c("rctrl")),
            nprobes=sum(str_starts(stimulus,"probe")),
            ntapother=sum(!is.na(response))-ntap-nprobes,
            mprobe1=mean(as.integer(response[stimulus=="probe1"])),
            mprobe2=mean(as.integer(response[stimulus=="probe2"])),
            mprobe3=mean(as.integer(response[stimulus=="probe3"])))

## Save file                                                     #####
save(raw_data, file = "data/export/raw_data.Rdata")
