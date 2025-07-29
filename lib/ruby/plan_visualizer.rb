module Lantae
  class PlanVisualizer
    def self.generate_ascii_graph(plan)
      output = []
      
      # Header
      output << "┌─" + "─" * 78 + "┐"
      output << "│ 📋 EXECUTION PLAN: #{plan['objective'].ljust(60)} │"
      output << "├─" + "─" * 78 + "┤"
      
      if plan['estimated_duration']
        output << "│ ⏱️  Duration: #{plan['estimated_duration'].ljust(66)} │"
      end
      
      if plan['success_criteria']
        output << "│ 🎯 Success Criteria:".ljust(79) + " │"
        plan['success_criteria'].each do |criteria|
          output << "│   • #{criteria.ljust(73)} │"
        end
      end
      
      output << "└─" + "─" * 78 + "┘"
      output << ""
      
      # Phase breakdown
      plan['phases'].each_with_index do |phase, phase_idx|
        draw_phase(output, phase, phase_idx, plan['phases'].size)
      end
      
      # Resource requirements
      if plan['resource_requirements']
        draw_resources(output, plan['resource_requirements'])
      end
      
      # Risks
      if plan['risks'] && plan['risks'].any?
        draw_risks(output, plan['risks'])
      end
      
      output.join("\n")
    end
    
    private
    
    def self.draw_phase(output, phase, phase_idx, total_phases)
      is_last_phase = phase_idx == total_phases - 1
      
      # Phase header
      phase_num = phase_idx + 1
      output << "┌─ Phase #{phase_num}: #{phase['name']}"
      output << "│"
      
      if phase['description']
        wrapped_desc = wrap_text(phase['description'], 76)
        wrapped_desc.each do |line|
          output << "│ #{line.ljust(76)} │"
        end
        output << "│"
      end
      
      # Tasks
      if phase['tasks'] && phase['tasks'].any?
        parallel_tasks = phase['tasks'].select { |t| t['parallel'] }
        sequential_tasks = phase['tasks'].reject { |t| t['parallel'] }
        
        # Draw parallel tasks first
        if parallel_tasks.any?
          output << "│ 🔄 Parallel Tasks:"
          draw_parallel_tasks(output, parallel_tasks)
        end
        
        # Draw sequential tasks
        if sequential_tasks.any?
          if parallel_tasks.any?
            output << "│"
            output << "│ ⬇️  Sequential Tasks:"
          end
          draw_sequential_tasks(output, sequential_tasks)
        end
      end
      
      # Phase connector
      if is_last_phase
        output << "└─" + "─" * 78
      else
        output << "├─" + "─" * 78
        output << "│"
        output << "▼ Phase #{phase_num + 1}"
        output << "│"
      end
      
      output << ""
    end
    
    def self.draw_parallel_tasks(output, tasks)
      tasks.each_with_index do |task, idx|
        is_last = idx == tasks.size - 1
        
        if idx == 0
          output << "│     ┌─ #{task['name']}"
        else
          output << "│     ├─ #{task['name']}"
        end
        
        if task['description'] && task['description'] != task['name']
          desc_lines = wrap_text("   #{task['description']}", 70)
          desc_lines.each do |line|
            prefix = is_last && line == desc_lines.last ? "│       " : "│     │ "
            output << "#{prefix}#{line}"
          end
        end
        
        if task['estimated_duration']
          duration_line = "   ⏱️  #{task['estimated_duration']}"
          prefix = is_last ? "│       " : "│     │ "
          output << "#{prefix}#{duration_line}"
        end
        
        if is_last
          output << "│     └─"
        end
      end
    end
    
    def self.draw_sequential_tasks(output, tasks)
      tasks.each_with_index do |task, idx|
        # Task box
        output << "│ ┌─ #{task['name']}"
        
        if task['description'] && task['description'] != task['name']
          desc_lines = wrap_text("#{task['description']}", 72)
          desc_lines.each do |line|
            output << "│ │  #{line.ljust(72)} │"
          end
        end
        
        # Task metadata
        metadata = []
        metadata << "⏱️  #{task['estimated_duration']}" if task['estimated_duration']
        metadata << "🧠 #{task['agent_type']}" if task['agent_type']
        
        if task['dependencies'] && task['dependencies'].any?
          dep_text = "📋 Depends on: #{task['dependencies'].join(', ')}"
          metadata << dep_text
        end
        
        metadata.each do |meta|
          output << "│ │  #{meta.ljust(72)} │"
        end
        
        output << "│ └─" + "─" * 74
        
        # Arrow to next task (except for last)
        unless idx == tasks.size - 1
          output << "│   ⬇️"
        end
      end
    end
    
    def self.draw_resources(output, resources)
      output << "┌─ 🛠️  RESOURCE REQUIREMENTS"
      output << "│"
      
      if resources['tools'] && resources['tools'].any?
        output << "│ 🔧 Tools: #{resources['tools'].join(', ')}"
      end
      
      if resources['models']
        output << "│ 🧠 Models:"
        output << "│   • Preferred: #{resources['models']['preferred']}"
        if resources['models']['alternatives'] && resources['models']['alternatives'].any?
          output << "│   • Alternatives: #{resources['models']['alternatives'].join(', ')}"
        end
      end
      
      if resources['external_apis'] && resources['external_apis'].any?
        output << "│ 🌐 External APIs: #{resources['external_apis'].join(', ')}"
      end
      
      output << "└─" + "─" * 78
      output << ""
    end
    
    def self.draw_risks(output, risks)
      output << "┌─ ⚠️  RISK ASSESSMENT"
      output << "│"
      
      risks.each do |risk|
        severity_color = case risk['impact']
        when 'high' then '🔴'
        when 'medium' then '🟡'  
        else '🟢'
        end
        
        output << "│ #{severity_color} #{risk['description']}"
        output << "│   Probability: #{risk['probability']} | Impact: #{risk['impact']}"
        output << "│   Mitigation: #{risk['mitigation']}"
        output << "│"
      end
      
      output << "└─" + "─" * 78
      output << ""
    end
    
    def self.wrap_text(text, width)
      return [text] if text.length <= width
      
      words = text.split(' ')
      lines = []
      current_line = ""
      
      words.each do |word|
        if current_line.empty?
          current_line = word
        elsif (current_line + " " + word).length <= width
          current_line += " " + word
        else
          lines << current_line
          current_line = word
        end
      end
      
      lines << current_line unless current_line.empty?
      lines
    end
  end
end