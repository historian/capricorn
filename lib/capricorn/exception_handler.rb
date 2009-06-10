
module Capricorn
  module ExceptionHandler
    
    def self.setup(out=STDOUT, err=STDERR)
      if String === out
        @out    = Logger.new(out, 'daily')
        @stdout = @out.instance_variable_get('@logdev').instance_variable_get('@dev')
      else
        @out    = Logger.new(out)
        @stdout = out
      end
      
      if String === err
        @err = Logger.new(err, 'daily')
        @stderr = @err.instance_variable_get('@logdev').instance_variable_get('@dev')
      else
        @err    = Logger.new(err)
        @stderr = err
      end
      
      @out.level = Logger::DEBUG
      @err.level = Logger::DEBUG
    end
    
    def self.err
      @err
    end
    
    def self.out
      @out
    end
    
    def self.stderr
      @stderr
    end
    
    def self.stdout
      @stdout
    end
    
    def self.redirect_std
      if STDOUT != self.stdout
        STDOUT.reopen self.stdout
      end
      
      if STDERR != self.stderr
        STDERR.reopen self.stderr
      end
      
      STDIN.reopen "/dev/null"
    end
    
    def logger
      Capricorn::ExceptionHandler
    end
    
    def log(*args, &block)
      args = [args.inspect] if args.size == 1 and Array === args.first
      logger.out.info(*args, &block)
    end
    
    def report
      yield
    rescue Exception => e
      if StandardError === e
        logger.err.error(e)
      else
        logger.err.fatal(e)
      end
      raise e
    end
    
  end
end

def FileUtils.fu_output_message(msg)
  Capricorn::ExceptionHandler.out.info(msg)
end
