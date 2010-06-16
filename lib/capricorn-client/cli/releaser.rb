class Capricorn::CLI::Releaser < Capricorn::CLI
  include Capricorn::Helpers

  namespace :release

  desc "patch", "release a new patch"
  def patch
    release_version(:patch)
  end

  desc "minor", "release a new minor"
  def minor
    release_version(:minor)
  end

  desc "major", "release a new major"
  def major
    release_version(:major)
  end

private

  def release_version(level)
    invoke "build:#{level}"
    if $last_version
      invoke "deploy:version", [$last_version]
    end
  end

end