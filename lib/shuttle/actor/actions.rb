
module Shuttle
  class Actor
    module Actions
      
      def self.included(base)
        base.extend Shuttle::Actor::Actions::ClassMethods
      end
      
      def run_callbacks!(action)
        action = action.to_sym
        self.class.methods_for(action, :before).each { |meth| self.send(meth) }
        self.class.methods_for(action, :on).each     { |meth| self.send(meth) }
        self.class.methods_for(action, :after).each  { |meth| self.send(meth) }
      end
      
      module ClassMethods
        
        def before(action, meth)
          methods_for(action, :before).push(meth.to_sym)
        end
        
        def on(action, meth)
          methods_for(action, :on).push(meth.to_sym)
        end
        
        def after(action, meth)
          methods_for(action, :after).push(meth.to_sym)
        end
        
        def methods_for(action, fase)
          action = action.to_sym
          fase   = fase.to_sym
          
          @actions ||= {}
          @actions[action] ||= {}
          @actions[action][fase] ||= []
          @actions[action][fase]
        end
        
        def action(*names)
          names.each do |name|
            module_eval %{
              def self.before_#{name}(meth)
                before(:#{name}, meth)
              end
              def self.on_#{name}(meth)
                on(:#{name}, meth)
              end
              def self.after_#{name}(meth)
                after(:#{name}, meth)
              end
              def run_#{name}_callbacks!
                run_callbacks!(:#{name})
              end
            }
          end
        end
        
      end
    end
  end
end