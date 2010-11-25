class Capricorn::CLI::Deployer < Capricorn::CLI
  include Capricorn::Helpers

  namespace :deploy

  desc "version [VERSION]", "build the current version"
  def version(version=nil)
    gem = nil
    if version
      gem = Dir.glob("pkg/*-#{version}.gem").last
    else
      gem = Dir.glob("pkg/*.gem").sort do |a, b|
        File.stat(a).mtime <=> File.stat(b).mtime
      end.last
    end

    unless gem
      halt "Please build a gem first!"
    end

    $capr_gems_weak_push = true

    invoke "gems:push", [gem]
    invoke "apps:fupdate"
  end

end