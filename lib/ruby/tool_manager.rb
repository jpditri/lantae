# Simplified tool manager for LSP components
# This is a stub that provides the minimal interface needed by LSP

class ToolManager
  def initialize
    @tools = {}
  end

  def register_tool(name, &block)
    @tools[name] = block
  end

  def execute_tool(name, *args)
    return { success: false, error: "Tool not found: #{name}" } unless @tools[name]
    
    begin
      result = @tools[name].call(*args)
      { success: true, result: result }
    rescue => e
      { success: false, error: e.message }
    end
  end

  def list_tools
    @tools.keys
  end

  def has_tool?(name)
    @tools.key?(name)
  end
end