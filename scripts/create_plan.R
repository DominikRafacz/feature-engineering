plan <- drake_plan(
  raw_data = obtain_all_data(),
  save_data = write.csv(raw_data, file_out("data/data_concataned.csv"), row.names = FALSE),
  splitted_data = split_data(raw_data),
  basic_benchmark = make_basic_benchmark(raw_data)
)