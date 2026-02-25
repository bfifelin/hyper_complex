# frozen_string_literal: true

require 'minitest/test_task'
require File.expand_path("#{File.dirname(__FILE__)}/lib/version.rb")

Minitest::TestTask.create(:test) do |t|
  t.libs << 'test'
  t.libs << 'lib'
  t.test_globs = ['test/**/*_test.rb']
end

task build: :gendoc do
  system 'gem build hyper_complex.gemspec'
end

task :gendoc do
  system 'yardoc'
end

task release: :build do
  system "gem push hyper_complex-#{HyperComplex::VERSION}.gem"
end

task default: :build
