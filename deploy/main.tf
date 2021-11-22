module "ep_common" {
  source = "git::git@github.com:EducationPerfect/infra-terraform-ep-common"
}

provider "aws" {
  region = module.ep_common.env.aws_region
  allowed_account_ids = module.ep_common.allowed_account_ids
}

# data "archive_file" "wkhtmltopfd" {
#   type        = "zip"
#   source_dir  = "${path.root}/assets/"
#   output_path = "wkhtmltopfd-lambda-layer.zip"
# }

resource "aws_lambda_layer_version" "lambda_layer" {
  layer_name          = "wkhtmltopfd"
  # filename            = data.archive_file.wkhtmltopfd.output_path
  filename            = "${path.root}/assets/wkhtmltopfd-lambda-layer.zip"
  # source_code_hash    = data.archive_file.wkhtmltopfd.output_base64sha256
  source_code_hash    = filebase64sha256("${path.root}/assets/wkhtmltopfd-lambda-layer.zip")
  compatible_runtimes = ["provided"]
}

# data "archive_file" "lambda" {
#   type        = "zip"
#   source_dir  = "${path.root}/assets/"
#   output_path = "pdf-template-lambda.zip"
# }

resource "aws_iam_role" "lambda" {
  name = "iam_for_lambda"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_lambda_function" "lambda" {
  # filename          = data.archive_file.lambda.output_path
  # source_code_hash  = data.archive_file.lambda.output_base64sha256
  filename          = "${path.root}/assets/pdf-template-lambda.zip"
  source_code_hash  = filebase64sha256("${path.root}/assets/pdf-template-lambda.zip")
  function_name     = "pdf-template-lambda"
  handler           = "NOT_USED"
  role              = aws_iam_role.lambda.arn
  layers            = [ aws_lambda_layer_version.lambda_layer.arn ]
  memory_size       = 128
  runtime           = "provided"
  timeout           = 30
  publish           = true
}

resource "aws_apigatewayv2_api" "lambda" {
  name          = "pdf_template_lambda_gw"
  protocol_type = "HTTP"
}

resource "aws_apigatewayv2_stage" "lambda" {
  api_id = aws_apigatewayv2_api.lambda.id

  name        = "pdf_template_lambda_stage"
  auto_deploy = true

  access_log_settings {
    destination_arn = aws_cloudwatch_log_group.api_gw.arn

    format = jsonencode({
      requestId               = "$context.requestId"
      sourceIp                = "$context.identity.sourceIp"
      requestTime             = "$context.requestTime"
      protocol                = "$context.protocol"
      httpMethod              = "$context.httpMethod"
      resourcePath            = "$context.resourcePath"
      routeKey                = "$context.routeKey"
      status                  = "$context.status"
      responseLength          = "$context.responseLength"
      integrationErrorMessage = "$context.integrationErrorMessage"
      }
    )
  }
}

resource "aws_apigatewayv2_integration" "lambda" {
  api_id = aws_apigatewayv2_api.lambda.id

  integration_uri    = aws_lambda_function.lambda.invoke_arn
  integration_type   = "AWS_PROXY"
  integration_method = "POST"
}

resource "aws_apigatewayv2_route" "lambda" {
  api_id = aws_apigatewayv2_api.lambda.id

  route_key = "GET /say_hello"
  target    = "integrations/${aws_apigatewayv2_integration.lambda.id}"
}

resource "aws_cloudwatch_log_group" "api_gw" {
  name = "/aws/api_gw/${aws_apigatewayv2_api.lambda.name}"

  retention_in_days = 30
}

resource "aws_lambda_permission" "api_gw" {
  statement_id  = "AllowExecutionFromAPIGateway"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.lambda.function_name
  principal     = "apigateway.amazonaws.com"

  source_arn = "${aws_apigatewayv2_api.lambda.execution_arn}/*/*"
}

output "base_url" {
  description = "Base URL for API Gateway stage."

  value = aws_apigatewayv2_stage.lambda.invoke_url
}
