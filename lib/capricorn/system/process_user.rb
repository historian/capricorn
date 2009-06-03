
module Capricorn
  class System
    module ProcessUser
      
      # get the uid for a user name
      def get_uid(username)
        return username if Numeric === username
        Etc.getpwnam(username).uid
      end
      
      # get the gid for a group name
      def get_gid(groupname)
        return groupname if Numeric === groupname
        Etc.getgrnam(groupname).gid
      end
      
      # get the user name for a uid (or the current process user)
      def get_user_name(uid=Process.uid)
        return uid if String === uid
        Etc.getpwuid(uid).name
      end
      
      # get the group name for a gid (or the current process group)
      def get_group_name(gid=Process.gid)
        return gid if String === gid
        Etc.getgrgid(gid).name
      end
      
      # is this process running as this user?
      def is_user(username)
        uid = get_uid(username)
        Process.euid == uid and Process.uid == uid
      end
      
      # is this process running as this group?
      def is_group(groupname)
        gid = get_gid(groupname)
        Process.egid == gid and Process.gid == gid
      end
      
      # switch this user to the specified user and group
      def switch_to_user(username, groupname=nil)
        different_uid = (Process.euid != get_uid(username))
        different_gid = (Process.egid != get_gid(groupname)) if groupname
        
        if groupname and different_gid
          Process.gid = Process.egid = get_gid(groupname)
        end
        
        if different_uid
          Process.uid = Process.euid = get_uid(username)
        end
      end
      
      # run the passed block as the specified user and group
      def as_user(username, groupname=nil, &block)
        euid = Process.euid
        egid = Process.egid
        
        value = nil
        begin
          switch_to_user(username, groupname)
          value = block.call
        ensure
          switch_to_user(euid, egid)
        end
        value
      end
      
    end
  end
end
