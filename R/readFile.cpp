#include <fstream>
#include <string>
#include <sstream>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector read_file_cpp(CharacterVector path) {
  std::string fname = as<std::string>(path);
  std::ifstream t(fname.c_str());
  std::stringstream buffer;
  buffer << t.rdbuf();
  return buffer.str();
}

// [[Rcpp::export]]
CharacterVector read_file_cpp2(CharacterVector path) {
  std::string fname = as<std::string>(path);
  std::ifstream in(fname.c_str());
  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);
  in.read(&contents[0], contents.size());
  in.close();
  return(contents);
}