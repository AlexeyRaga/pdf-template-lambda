{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.Lambda.Runtime (pureRuntime)
import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest(..), NoAuthorizer)
import AWS.Lambda.Events.ApiGateway.ProxyResponse (ProxyResponse(..), textPlain, forbidden403, ok200)
import Data.Text (pack)

import PdfTemplate.Worker

myHandler :: ProxyRequest NoAuthorizer -> ProxyResponse
myHandler ProxyRequest { httpMethod = "GET", path = "/say_hello" } =
    ProxyResponse
    {   status = ok200
    ,   body = textPlain "Hello"
    -- ,   headers = mempty
    ,   multiValueHeaders = mempty
    }
myHandler req =
    ProxyResponse
    {   status = forbidden403
    ,   body = textPlain (pack $ show req)
    -- ,   headers = mempty
    ,   multiValueHeaders = mempty
    }

main2 :: IO ()
main2 = pureRuntime myHandler

main :: IO ()
main = process "/tmp/2/template.zip"

