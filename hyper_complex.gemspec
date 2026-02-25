# frozen_string_literal: true

require File.expand_path('lib/version', __dir__)

Gem::Specification.new do |s|
  s.name        = 'hyper_complex'
  s.version     = HyperComplex::VERSION
  s.require_paths = ['lib']
  s.date        = '2026-02-01'
  s.summary     = 'Hypercomplex numbers'
  s.description = 'Hypercomplex numbers (by Cayley-Dickson construction)'
  s.authors     = ['Boris Fifelin']
  s.email       = 'bfifelin@gmail.com'
  s.homepage    = 'https://github.com/bfifelin/hyper_complex'
  s.licenses    = ['MIT']
  s.files       = Dir.glob('lib/*') + %w[LICENSE README.html README.md Rakefile .yardopts]
  s.required_ruby_version = '>= 3.1.0'
  s.add_development_dependency 'minitest', '>= 5.0'
  s.add_development_dependency 'rake', '>= 13.0'
  s.add_development_dependency 'yard', '>= 0.9'
end
