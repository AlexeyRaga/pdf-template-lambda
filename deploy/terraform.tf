terraform {
  required_version = ">= 1.0.0, < 1.1"

  # backend "s3" {
  #   bucket         = "tfstate.educationperfect.com"
  #   key            = "backstage-io"
  #   region         = "ap-southeast-2"
  #   profile        = "default"
  #   dynamodb_table = "ep-terraform-state"
  #   role_arn       = "arn:aws:iam::125159634937:role/terraform-state/backstage-io-backstage-io-tf-backend-access"
  # }

  backend "s3" {
    bucket         = "tfstate.educationperfect.com"
    key            = "services-platform"
    region         = "ap-southeast-2"
    profile        = "default"
    dynamodb_table = "ep-terraform-state"
    role_arn       = "arn:aws:iam::125159634937:role/services-platform-tf-backend-access-role"
  }

  required_providers {
    aws = {
      version = "3.64.2"
    }

    tls = {
      source  = "hashicorp/tls"
      version = "3.1.0"
    }
  }
}
