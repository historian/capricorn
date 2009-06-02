require 'thread'

module Shuttle
  class JobQueue
    include DRbUndumped
    
    # create a new job queue
    def initialize
      @immediated_jobs = Array.new
      @canceled_jobs = Array.new
      @job_queue = Array.new
      @jobs      = Hash.new
      @mutex     = Mutex.new
      
      @worker    = Thread.new(self) do |job_queue|
        while job_queue.running? or job_queue.peek
          
          if job = job_queue.peek
            job.run(job_queue)
            job_queue.delete(job.object_id)
          else
            sleep(1)
          end
          
        end
      end
    end
    
    # enqueue a new job with the given +name+, +options+ and +proc+
    def enqueue(name, options={}, &proc)
      @mutex.synchronize do
        job = Job.new(name, options, &proc)
        @jobs[job.object_id] = job
        @job_queue.push job.object_id
        return job.object_id
      end
    end
    
    # dequeue the next job of the queue.
    def dequeue
      @mutex.synchronize do
        id = @job_queue.shift
        return @jobs.delete(id) if id
      end
    end
    
    # delete the job associated with the given +id+.
    def delete(id)
      @mutex.synchronize do
        id = @job_queue.delete(id)
        return @jobs.delete(id) if id
      end
    end
    
    # peek at the next job in the queue
    def peek
      job = nil
      @mutex.synchronize do
        id = @job_queue.first
        job = @jobs[id] if id
      end
      job
    end
    
    # get the size of the job queue
    def size
      @mutex.synchronize do
        @job_queue.size
      end
    end
    
    # cancel the job associated with the given +id+.
    def cancel(id)
      @mutex.synchronize do
        id = @job_queue.delete(id)
        if id
          @jobs.delete(id)
          @canceled_jobs.push(id)
        end
      end
    end
    
    # is the job associated with the given +id+ canceled.
    def canceled?(id)
      @mutex.synchronize do
        return !@canceled_jobs.delete(id).nil?
      end
    end
    
    # run the job associated with the given +id+ immediately.
    def immediate(id)
      @mutex.synchronize do
        @immediated_jobs.push(id) if @jobs[id]
      end
    end
    
    # should the job associated with the given +id+ be run immediately.
    def immediated?(id)
      @mutex.synchronize do
        return !@immediated_jobs.delete(id).nil?
      end
    end
    
    # join the worker thread
    def join!
      @worker.join
    end
    
    # wait until the queue is empty then stop the worker
    def stop!
      @mutex.synchronize do
        @stopped = true
      end
      join!
    end
    
    # is the queue stopping or stopped?
    def stopped?
      @mutex.synchronize do
        return !!@stopped
      end
    end
    
    # is the queue running
    def running?
      !stopped?
    end
    
    # iterate through all the jobs on the queue
    def each
      @mutex.synchronize do
        @job_queue.each do |id|
          job        = @jobs[id]
          canceled   = @canceled_jobs.include?(id)
          immediated = @immediated_jobs.include?(id)
          
          yield(job, canceled, immediated)
        end
      end
    end
    
    class Job
      include DRbUndumped
      
      attr_accessor :name, :options, :proc
      
      def initialize(name, options={}, &proc)
        @mutex = Mutex.new
        @name, @options, @proc = name, options, proc
        @run_at = Time.now + (options.delete(:delay) || 30)
      end
      
      def delay
        @mutex.synchronize do
          delay = @run_at - Time.now
          delay = 0 if delay < 0
          return delay
        end
      end
      
      def running?
        @mutex.synchronize do
          return @running
        end
      end
      
      def waiting?
        @mutex.synchronize do
          return @waiting
        end
      end
      
      def run(job_queue)
        @waiting = true
        immediated = canceled = false
        Shuttle.log "waiting #{@run_at - Time.now}s."
        until immediated or canceled or @run_at <= Time.now
          sleep(1)
          canceled   = job_queue.canceled?(self.object_id)
          immediated = job_queue.immediated?(self.object_id)
        end
        
        unless canceled
          @waiting = false
          @running = true
          Shuttle.log("[queue]> #{@name}")
          @proc.call(@options)
        end
      end
      
    end
    
  end
end
