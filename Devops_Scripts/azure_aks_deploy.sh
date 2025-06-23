#! /bin/bash
#Vars
buildID=$1 
temp_dir=$2
env_Name=$3
namespace=$4
host_url=$host_url
ContainerRegistryURL=$registry_url
ACR_NAME=$acr_name
MIN_CPU=$minCPU
MIN_MEM=$minMem
MAX_CPU=$maxCPU
MAX_MEM=$maxMem
prod_namespace="runner-shipment-service-production"
deployment_name="runner-shipment-service-deployment"
HPA_NAME="runner-shipment-service-hpa"
USER_ASSIGNED_IDENTITY_ID=$user_identity_id

source_path="${PWD}/_runner_shipment_service/runner-shipment-service"
k8s_resource_path="${source_path}/Devops_Scripts/K8"
app_deployment_yaml_path="$k8s_resource_path/app_deployment.yml"
app_service_yaml_path="$k8s_resource_path/app_service.yml"
app_ingress_yaml_path="$k8s_resource_path/app_ingress.yml"
app_config_yaml_path="$k8s_resource_path/app_config.yml"
app_hpa_yaml_path="$k8s_resource_path/app-hpa.yaml"
secretproviderclass_yaml_path="${source_path}/Devops_Scripts/configs/${env_Name}/secretproviderclass.yaml"

#Replace vars in Ingress
sed -i "s/NAMESPACE_NAME/$namespace/g" $app_ingress_yaml_path
sed -i "s/HOST_URL/$host_url/g" $app_ingress_yaml_path

#Replace vars in Service
sed -i "s/NAMESPACE_NAME/$namespace/g" $app_service_yaml_path

#Replace vars in Config
sed -i "s/NAMESPACE_NAME/$namespace/g" $app_config_yaml_path
sed -i "s/ENV_NAME/$env_Name/g" $app_config_yaml_path
sed -i "s/license_key/$new_relic_license_key/g" $app_config_yaml_path

#Replace vars in Deployment
sed -i "s/MIN_CPU/$MIN_CPU/g" $app_deployment_yaml_path
sed -i "s/MIN_MEM/$MIN_MEM/g" $app_deployment_yaml_path
sed -i "s/MAX_CPU/$MAX_CPU/g" $app_deployment_yaml_path
sed -i "s/MAX_MEM/$MAX_MEM/g" $app_deployment_yaml_path
sed -i "s/NAMESPACE_NAME/$namespace/g" $app_deployment_yaml_path
sed -i "s/ContainerRegistryURL/$ContainerRegistryURL/g" $app_deployment_yaml_path
sed -i "s/ACR_NAME/$ACR_NAME/g" $app_deployment_yaml_path
sed -i "s/LATEST_IMAGE/$buildID/g" $app_deployment_yaml_path
sed -i "s/REPLICA_COUNT/$replica_count/g" $app_deployment_yaml_path
sed -i "s/HEAP_MIN/$HEAP_MIN/g" $app_deployment_yaml_path
sed -i "s/HEAP_MAX/$HEAP_MAX/g" $app_deployment_yaml_path
sed -i "s/JVM_ALG/$JVM_ALG/g" $app_deployment_yaml_path
sed -i "s/ENV_NAME/$env_Name/g" $app_deployment_yaml_path
sed -i "s/\$POOL/${POOL}/g" $app_deployment_yaml_path
sed -i "s/\$KEY/${KEY}/g" $app_deployment_yaml_path
sed -i "s/\$TYPE/${TYPE}/g" $app_deployment_yaml_path
sed -i "s/\$TOLERATION_KEY/${TOLERATION_KEY}/g" $app_deployment_yaml_path
sed -i "s/\$TOLERATION_POOL/${TOLERATION_POOL}/g" $app_deployment_yaml_path

# replace HPA variables
sed -i "s/\$HPA_NAME/${HPA_NAME}/g" $app_hpa_yaml_path
sed -i "s/\$HPA_MIN/${HPA_MIN}/g" $app_hpa_yaml_path
sed -i "s/\$HPA_MAX/${HPA_MAX}/g" $app_hpa_yaml_path
sed -i "s/\$NAMESPACE_NAME/$namespace/g" $app_hpa_yaml_path
sed -i "s/\$DEPLOYMENT_NAME/${deployment_name}/g" $app_hpa_yaml_path
sed -i "s/\$AVG_CPU_UTIL/${AVG_CPU_UTIL}/g" $app_hpa_yaml_path
sed -i "s/\$STABLE_SEC/${STABLE_SEC}/g" $app_hpa_yaml_path
sed -i "s/\$SCALE_UP_COUNT/${SCALE_UP_COUNT}/g" $app_hpa_yaml_path
sed -i "s/\$SCALE_UP_SEC/${SCALE_UP_SEC}/g" $app_hpa_yaml_path

# replace secretproviderclass variables
sed -i "s/\$USER_ASSIGNED_IDENTITY_ID/${USER_ASSIGNED_IDENTITY_ID}/g" $secretproviderclass_yaml_path
sed -i "s/\$NAMESPACE_NAME/${namespace}/g" $secretproviderclass_yaml_path

#Displaying file updates
echo "*****App Deployment*****"
cat $app_deployment_yaml_path ; echo
echo "*****Service manifest*****"
cat $app_service_yaml_path ; echo
echo "*****Ingress manifest*****"
cat $app_ingress_yaml_path ; echo
echo "*****Configmap*****"
cat $app_config_yaml_path ; echo
echo "*****HPA*****"
cat $app_hpa_yaml_path ; echo
echo "*****Secret Provider Class*****"
cat $secretproviderclass_yaml_path ; echo

#Kube configs
kubectl_prod="kubectl --kubeconfig ${temp_dir}/kubeconfig_prod"
kubectl_nonprod="kubectl --kubeconfig ${temp_dir}/kubeconfig_nonprod"

if [ "$namespace" = "$prod_namespace" ]; then
    echo "This is PROD deployment. Using prod kube config -> $kubectl_prod"
    kubectl_current=$kubectl_prod
else
    echo "This is NON PROD deployment. Using non prod kube config -> $kubectl_nonprod"
    kubectl_current=$kubectl_nonprod
fi

#Namespace
if [[ ! `${kubectl_current} get namespaces | grep $namespace` ]]; then
    echo "*****Namespace not present. Plase create namespace first*****"
    exit 1
else
    echo "*****Namespace already present*****"
fi

#Config
${kubectl_current} apply -f "$app_config_yaml_path"
if [[ $? -eq 0 ]]; then
    echo "*****Service configure success !!!*****"
else
    echo "*****Service configure fail !!!*****"
    exit 1
fi

#Service
${kubectl_current} apply -f "$app_service_yaml_path"
if [[ $? -eq 0 ]]; then
    echo "*****Service configure success !!!*****"
else
    echo "*****Service configure fail !!!*****"
    exit 1
fi

#Ingress
${kubectl_current} apply -f "$app_ingress_yaml_path"
if [[ $? -eq 0 ]]; then
    echo "*****Ingress configure success !!!*****"
else
    echo "*****Ingress configure fail !!!*****"
    exit 1
fi

#HPA
${kubectl_current} apply -f "$app_hpa_yaml_path"
if [[ $? -eq 0 ]]; then
    echo "*****HPA configure success !!!*****"
else
    echo "*****HPA configure fail !!!*****"
    exit 1
fi

#KV
${kubectl_current} apply -f "$secretproviderclass_yaml_path"
if [[ $? -eq 0 ]]; then
    echo "*****KV configure success !!!*****"
else
    echo "*****KV configure fail !!!*****"
    exit 1
fi

#Deployment
${kubectl_current} apply -f "$app_deployment_yaml_path"
${kubectl_current} rollout status deployment.apps/$deployment_name --namespace $namespace
if [[ $? -eq 0 ]]; then
    echo "*****App Deployment success !!!*****"
else
    echo "*****App Deployment fail !!!*****"
    exit 1
fi
service_yaml_path="${PWD}/_runner_shipment_service/runner-shipment-service/Devops_Scripts/service-values.yaml"
values=$(grep ":" "$service_yaml_path" | awk -F ": " '{print $1"="$2}')
${kubectl_current} label --overwrite namespace $namespace $values