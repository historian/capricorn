Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps
    
    class Jobs < Thor
      namespace :jobs
      
      class_option :token,
        :desc => 'Name or path of a token.',
        :banner => 'name',
        :type => :string,
        :required => false,
        :aliases => %w( -t )
      
      desc "list", 'list the jobs in the queue'
      def list
        queued_jobs = Capricorn.client(options[:token]).queued_jobs
        queued_jobs.each do |id, name, canceled, immediated, running, waiting, delay|
          status = []
          status.push canceled   ? 'c' : ' '
          status.push immediated ? 'i' : ' '
          status.push running    ? 'r' : ' '
          status.push waiting    ? 'w' : ' '
          puts("% 8d  % 8d  % 8s  %s" % [id, delay.to_i, status.join, name])
        end
      end
      
      desc "cancel ID", 'cancel the job with ID'
      def cancel(id)
        Capricorn.client(options[:token]).cancel_job(id.to_i)
      end
      
      desc "immediate ID", 'immediately run the job with ID'
      def immediate(id)
        Capricorn.client(options[:token]).immediate_job(id.to_i)
      end
      
    end
    
  end
end