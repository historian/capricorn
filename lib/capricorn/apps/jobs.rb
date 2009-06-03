Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps
    class Jobs < Thor
      
      desc "list", 'list the jobs in the queue'
      method_options :token => :optional
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
      method_options :token => :optional
      def cancel(id)
        Capricorn.client(options[:token]).cancel_job(id.to_i)
      end
      
      desc "immediate ID", 'immediately run the job with ID'
      method_options :token => :optional
      def immediate(id)
        Capricorn.client(options[:token]).immediate_job(id.to_i)
      end
      
    end
  end
end