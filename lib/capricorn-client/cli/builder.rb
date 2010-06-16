class Capricorn::CLI::Builder < Capricorn::CLI
  include Capricorn::Helpers

  namespace :build

  desc "current", "build the current version"
  def current
    system("rake build")
  end

  desc "patch", "build a new patch"
  def patch
    check_clean_stage
    bump_version(:patch)
  end

  desc "minor", "build a new minor"
  def minor
    check_clean_stage
    bump_version(:minor)
  end

  desc "major", "build a new major"
  def major
    check_clean_stage
    bump_version(:major)
  end

private

  def check_clean_stage
    unless %x[ git status 2>&1 ].include?('nothing to commit (working directory clean)')
      halt("Your git stage is not clean!")
    end
  end

  def bump_version(level)
    system("rake version:bump:#{level} build")

    if %x[ rake version ] =~ /^Current\sversion[:]\s([\d.]+)$/im
      $last_version = $1
      system("git tag -a -m \"#{$last_version}\" #{$last_version}")
      system("git push origin master")
      system("git push origin master --tags")
    end
  end

end