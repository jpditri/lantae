#!/usr/bin/env ruby

require_relative 'server'

# Run the LSP server
server = Lantae::LSP::Server.new
server.run