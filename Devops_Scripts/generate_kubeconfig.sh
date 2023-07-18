#! /bin/bash
temp_dir=$1
#Prod kubeconfig
az aks get-credentials --resource-group $resourceGroupName --name $clusterName --overwrite-existing -f ${temp_dir}/kubeconfig_prod

#Non prod kubeconfig
az aks get-credentials --resource-group $resourceGroupName --name $clusterName --overwrite-existing -f ${temp_dir}/kubeconfig_nonprod