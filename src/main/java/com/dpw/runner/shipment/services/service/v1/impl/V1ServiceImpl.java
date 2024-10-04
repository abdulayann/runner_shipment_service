package com.dpw.runner.shipment.services.service.v1.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CheckActiveInvoiceRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CheckTaskExistV1Request;
import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateV1ConsolidationTaskFromV2Request;
import com.dpw.runner.shipment.services.dto.v1.request.CreateV1ShipmentTaskFromV2Request;
import com.dpw.runner.shipment.services.dto.v1.request.CreditLimitValidateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1RoleIdRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.AddressTranslationListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CheckActiveInvoiceResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CompanySettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ConsoleBookingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CreditLimitValidateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.HblTaskCreationResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantDetailsByListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.UpdateOrgCreditLimitBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.UsersRoleListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RoleIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.impl.GetUserServiceFactory;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.TokenUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;

@Service
//@EnableAsync
public class V1ServiceImpl implements IV1Service {

    private static final Logger log = LoggerFactory.getLogger(V1ServiceImpl.class);
    public static final String UN_AUTHORIZED_EXCEPTION_STRING = "UnAuthorizedException";
    public static final String REQUEST_ID_FOR_EVENT_ACTUAL_TIME_TAKEN_MS_MSG = " RequestId: {} || {} for event: {} Actual time taken: {} ms";
    public static final String TOKEN_TIME_TAKEN_IN_SEND_SHIPMENT_TASK_FUNCTION_MSG = "Token time taken in sendShipmentTask() function ";
    public static final String TOKEN_TIME_TAKEN_IN_FETCH_SHIPMENT_BILLING_DATA_FUNCTION_MSG = "Token time taken in fetchShipmentBillingData() function ";
    public static final String TOKEN_TIME_TAKEN_IN_FETCH_ROLES_LIST_FUNCTION_MSG = "Token time taken in fetchRolesList() function ";
    public static final String REQUEST_TOTAL_TIME_TAKEN_TO_GET_MAX_SHIPMENT_ID_MSG = "Request: {} || Total time taken to get max shipment id: {}";
    public static final String JOIN_REGEX = "{} {}";

    private RestTemplate restTemplate;
    private GetUserServiceFactory getUserServiceFactory;
    private TokenUtility tokenUtility;

    @Value("${v1service.url.base}${v1service.url.customerBooking}")
    private String CUSTOMER_BOOKING_URL;

    @Value("${v1service.url.base}${v1service.url.updateOrgCreditLimit}")
    private String UPDATE_ORG_CREDIT_LIMIT;

    @Value("${v1service.url.base}${v1service.url.masterData}")
    private String MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.masterDataCreate}")
    private String MASTER_DATA_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.masterDataUpdate}")
    private String MASTER_DATA_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.carrierData}")
    private String CARRIER_MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.carrierOrgRefFilteredData}")
    private String CARRIER_MASTER_DATA_ORG_REF_FILTER_URL;

    @Value("${v1service.url.base}${v1service.url.carrierDataCreate}")
    private String CARRIER_MASTER_DATA_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.carrierDataUpdate}")
    private String CARRIER_MASTER_DATA_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.containerTypeData}")
    private String CONTAINER_TYPE_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.containerTypeCreate}")
    private String CONTAINER_TYPE_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.containerTypeUpdate}")
    private String CONTAINER_TYPE_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.vesselData}")
    private String VESSEL_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.vesselCreate}")
    private String VESSEL_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.vesselUpdate}")
    private String VESSEL_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.routingMasterData}")
    private String ROUTING_MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.routingMasterCreate}")
    private String ROUTING_MASTER_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.routingMasterUpdate}")
    private String ROUTING_MASTER_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.currenciesData}")
    private String CURRENCIES_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.currenciesCreate}")
    private String CURRENCIES_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.currenciesUpdate}")
    private String CURRENCIES_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.dangerousGoodData}")
    private String DANGEROUS_GOOD_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.dangerousGoodCreate}")
    private String DANGEROUS_GOOD_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.dangerousGoodUpdate}")
    private String DANGEROUS_GOOD_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.warehouseData}")
    private String WAREHOUSE_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.warehouseCreate}")
    private String WAREHOUSE_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.warehouseUpdate}")
    private String WAREHOUSE_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.portsData}")
    private String PORTS_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.portsCreate}")
    private String PORTS_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.portsUpdate}")
    private String PORTS_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.commodityData}")
    private String COMMODITY_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.commodityCreate}")
    private String COMMODITY_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.commodityUpdate}")
    private String COMMODITY_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.salesAgentData}")
    private String SALES_AGENT_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.salesAgentCreate}")
    private String SALES_AGENT_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.salesAgentUpdate}")
    private String SALES_AGENT_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.unlocation}")
    private String UNLOCATION_URL;

    @Value("${v1service.url.base}${v1service.url.organization}")
    private String ORGANIZATION_API;

    @Value("${v1service.url.base}${v1service.url.unlocationCreate}")
    private String UNLOCATION_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.unlocationUpdate}")
    private String UNLOCATION_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.organizationCreate}")
    private String ORGANIZATION_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.organizationUpdate}")
    private String ORGANIZATION_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.multipleMasterData}")
    private String MULTIPLE_MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.userData}")
    private String USER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.gridColorCodeData}")
    private String GRID_COLOR_CODE_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.gridColorCodeCreate}")
    private String GRID_COLOR_CODE_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.gridColorCodeUpdate}")
    private String GRID_COLOR_CODE_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.listCousinBranch}")
    private String LIST_COUSIN_BRANCH_URL;

    @Value("${v1service.url.base}${v1service.url.ListCousinBranchWithoutCurrent}")
    private String LIST_COUSIN_BRANCH_WITHOUT_CURRENT_URL;

    @Value("${v1service.url.base}${v1service.url.TenantIdByGuid}")
    private String TENANT_BY_GUID_URL;

    @Value("${v1service.url.base}${v1service.url.sendConsolidationTask}")
    private String SEND_CONSOLIDATION_TASK_URL;

    @Value("${v1service.url.base}${v1service.url.sendShipmentTask}")
    private String SEND_SHIPMENT_TASK_URL;

    @Value("${v1service.url.base}${v1service.url.sendV1ConsolidationTask}")
    private String SEND_V1_CONSOLIDATION_TASK_URL;

    @Value("${v1service.url.base}${v1service.url.sendV1ShipmentTask}")
    private String SEND_V1_SHIPMENT_TASK_URL;
    @Value("${v1service.url.base}${v1service.url.checkTaskExist}")
    private String CHECK_TASK_EXIST;

    @Value("${v1service.url.base}${v1service.url.importFlightSchedules}")
    private String IMPORT_FLIGHT_SCHEDULE;

    @Value("${v1service.url.base}${v1service.url.fetchFlightStatus}")
    private String FETCH_FLIGHT_STATUS;

    @Value("${v1service.url.base}${v1service.url.importSailingSchedules}")
    private String IMPORT_SAILING_SCHEDULES;

    @Value("${v1service.url.base}${v1service.url.listSailingSchedule}")
    private String LIST_SAILING_SCHEDULE;

    @Value("${v1service.url.base}${v1service.url.addressList}")
    private String ADDRESS_LIST;

    @Value("${v1service.url.base}${v1service.url.tenantNameByTenantId}")
    private String TENANT_NAME_BY_ID;

    @Value("${v1service.url.base}${v1service.url.chargeType}")
    private String CHARGE_TYPE_URL;
    @Value("${v1service.url.base}${v1service.url.retrieveChargeType}")
    private String RETRIEVE_CHARGE_TYPE_URL;

    @Value("${v1service.url.base}${v1service.url.retrieveTenantSettings}")
    private String RETRIEVE_TENANT_SETTINGS;

    @Value("${v1service.url.base}${v1service.url.companySettings}")
    private String RETRIEVE_COMPANY_SETTINGS;

    @Value("${v1service.url.base}${v1service.url.unlocationOriginAndDestinationList}")
    private String UNLOCATION_ORIGIN_AND_DESTINATION_LIST_URL;

    @Value("${v1service.url.base}${v1service.url.listUnlocationTransportModeBased}")
    private String LIST_UNLOCATION_TRANSPORT_MODE_BASED_URL;

    @Value("${v1service.url.base}${v1service.url.activityMaster}")
    private String ACTIVITY_MASTER_URL;

    @Value("${v1service.url.base}${v1service.url.retrieveTenant}")
    private String RETRIEVE_TENANT;

    @Value("${v1service.url.base}${v1service.url.getDefaultOrg}")
    private String GET_DEFAULT_ORG;

    @Value("${v1service.url.base}${v1service.url.ownType}")
    private String OWN_TYPE;

    @Value("${v1service.url.base}${v1service.url.carrierFilterList}")
    private String CARRIER_FILTER_LIST;

    @Value("${v1service.url.base}${v1service.url.transportInstructionList}")
    private String TRANSPORT_INSTRUCTION_LIST;

    @Value("${v1service.url.base}${v1service.url.containerTransportInstructionList}")
    private String CONTAINER_TRANSPORT_INSTRUCTION_LIST;

    @Value("${v1service.url.base}${v1service.url.consolidationBookingData}")
    private String CONSOLIDATION_BOOKING_DATA;

    @Value("${v1service.url.base}${v1service.url.shipmentBillingData}")
    private String SHIPMENT_BILLING_DATA;

    @Value("${v1service.url.base}${v1service.url.shipmentWayBillFilter}")
    private String SHIPMENT_WAY_BILL_FILTER;

    @Value("${v1service.url.base}${v1service.url.consolidationBookingIdFilter}")
    private String CONSOLIDATION_BOOKING_ID_FILTER;

    @Value("${v1service.url.base}${v1service.url.mainPageTemplate}")
    private String MAIN_PAGE_TEMPLATE_LIST;
    @Value("${v1service.url.base}${v1service.url.hblTaskCreation}")
    private String HBL_TASK_CREATION;
    @Value("${v1service.url.base}${v1service.url.roleList}")
    private String ROLES_LIST;
    @Value("${v1service.url.base}${v1service.url.billingData}")
    private String BILLING_LIST;
    @Value("${v1service.url.base}${v1service.url.billChargesData}")
    private String BILL_CHARGES_LIST;
    @Value("${v1service.url.base}${v1service.url.arObjectData}")
    private String AR_OBJECT_LIST;
    @Value("${v1service.url.base}${v1service.url.dataSync}")
    private String DATA_SYNC_URL;
    @Value("${v1service.url.base}${v1service.url.getMaxShipmentId}")
    private String GET_MAX_SHIPMENT_ID_URL;
    @Value("${v1service.url.base}${v1service.url.getShipmentSequenceNumber}")
    private String GET_SHIPMENT_SEQUENCE_NUMBER_URL;
    @Value("${v1service.url.base}${v1service.url.getMaxConsolidationId}")
    private String GET_MAX_CONSOL_ID_URL;
    @Value("${v1service.url.base}${v1service.url.shipmentRetrieve}")
    private String SHIPMENT_RETRIEVE_URL;
    @Value("${v1service.url.base}${v1service.url.creditLimitCheck}")
    private String CREDIT_LIMIT_CHECK_URL;
    @Value("${v1service.url.base}${v1service.url.getAddressTranslation}")
    private String GET_ADDRESS_TRANSLATION;
    @Value("${v1service.url.base}${v1service.url.fetchActiveInvoices}")
    private String GET_ACTIVE_INVOICES;
    @Value("${v1service.url.base}${v1service.url.creditLimit}")
    private String CREDIT_LIMIT_LIST;
    @Value("${v1service.url.base}${v1service.url.org-address-list}")
    private String ORG_ADDRESS_LIST;
    @Value("${v1service.url.base}${v1service.url.addressRetrieve}")
    private String ADDRESS_RETRIEVE;
    @Value("${v1service.url.base}${v1service.url.getColoadingStations}")
    private String GET_CO_LOAD_STATIONS;
    @Value("${v1service.url.base}${v1service.url.getTenantDetails}")
    private String getTenantInfoUrl;
    @Value("${v1service.url.base}${v1service.url.getEmailTemplates}")
    private String getEmailTemplates;
    @Value("${v1service.url.base}${v1service.url.getMasterDetails}")
    private String getMasterDetails;
    @Value("${v1service.url.base}${v1service.url.getUserDetails}")
    private String getUserDetails;
    @Value("${v1service.url.base}${v1service.url.createTask}")
    private String createTaskUrl;
    @Value("${v1service.url.base}${v1service.url.updateTask}")
    private String updateTaskUrl;
    @Value("${v1service.url.base}${v1service.url.retrieveTask}")
    private String retrieveTaskUrl;
    @Value("${v1service.url.base}${v1service.url.listTask}")
    private String listTaskUrl;
    @Value("${v1service.url.base}${v1service.url.userEmails}")
    private String getUserEmailsByRoleId;

    @Value("${v1service.url.base}${v1service.url.rolesIdByRoleName}")
    private String getRolesIdByRoleName;

    @Value("${v1service.url.base}${v1service.url.generateToken}")
    private String v1GenerateTokenUrl;

    @Value("${v1service.serviceAccount.username}")
    private String serviceAccountUsername;

    @Value("${v1service.serviceAccount.password}")
    private String serviceAccountPassword;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private V1AuthHelper v1AuthHelper;
    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    public V1ServiceImpl(@Qualifier("restTemplateForV1") RestTemplate restTemplate,
            GetUserServiceFactory getUserServiceFactory, TokenUtility tokenUtility) {
        this.restTemplate = restTemplate;
        this.getUserServiceFactory = getUserServiceFactory;
        this.tokenUtility = tokenUtility;
    }

    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ResponseEntity<V1ShipmentCreationResponse> createBooking(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled, UUID shipmentGuid, HttpHeaders headers) {
        try {
            var request = v1ServiceUtil.createBookingRequestForV1(customerBooking, isShipmentEnabled, isBillingEnabled, shipmentGuid);
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, Objects.isNull(headers) ? V1AuthHelper.getHeaders() : headers);
            log.info("Payload sent for event: {} with request payload: {}", IntegrationType.V1_SHIPMENT_CREATION, jsonHelper.convertToJson(request));
            ResponseEntity<V1ShipmentCreationResponse> response = this.restTemplate.postForEntity(this.CUSTOMER_BOOKING_URL, entity, V1ShipmentCreationResponse.class, new Object[0]);
            log.info("Response received from V1 for event: {} with response: {} for booking: {}", IntegrationType.V1_SHIPMENT_CREATION, jsonHelper.convertToJson(response),  jsonHelper.convertToJson(request));
            return response;
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            log.info("Error Response from V1 for event: {} with exception: {}", IntegrationType.V1_SHIPMENT_CREATION, ex.getMessage());
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception exception) {
            log.info("Error Response from V1 for event: {} with exception: {}", IntegrationType.V1_SHIPMENT_CREATION, exception.getMessage());
            throw new V1ServiceException(exception.getMessage());
        }
    }

    public ResponseEntity<UpdateOrgCreditLimitBookingResponse> updateOrgCreditLimitFromBooking(CheckCreditLimitResponse request) {
        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            log.info("Payload sent for event: {} with request payload: {}", IntegrationType.V1_ORG_CREDIT_LIMIT_CREATION, jsonHelper.convertToJson(request));
            return this.restTemplate.postForEntity(this.UPDATE_ORG_CREDIT_LIMIT, entity, UpdateOrgCreditLimitBookingResponse.class, new Object[0]);
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public void setAuthContext() {
        String token = "Bearer "+ StringUtils.defaultString(generateToken());
        UsersDto user = getUserServiceFactory.returnUserService().getUserByToken(tokenUtility.getUserIdAndBranchId(token), token);
        if (user != null) {
            setAuthContext(token, user);
        }
    }

    private void setAuthContext(String token, UsersDto user) {
        RequestAuthContext.setAuthToken(token);
        TenantContext.setCurrentTenant(user.getTenantId());
        UserContext.setUser(user);
        List<String> grantedPermissions = new ArrayList<>(user.getPermissions().keySet());
        UsernamePasswordAuthenticationToken authenticationToken =
                new UsernamePasswordAuthenticationToken(user, null, getAuthorities(grantedPermissions));
        SecurityContextHolder.getContext().setAuthentication(authenticationToken);

        PermissionsContext.setPermissions(grantedPermissions);
    }

    private Collection<? extends GrantedAuthority> getAuthorities(List<String> permissions) {
        return permissions.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList());
    }

    @Override
    public void clearAuthContext() {
        TenantContext.removeTenant();
        RequestAuthContext.removeToken();
        PermissionsContext.removePermissions();
        SecurityContextHolder.clearContext();
        UserContext.removeUser();
    }

    @Override
    public String generateToken() {
        try {
            long startTime = System.currentTimeMillis();

            // Set headers
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);

            // Create the request body
            Map<String, String> requestBody = new HashMap<>();
            requestBody.put("UserName", serviceAccountUsername);
            requestBody.put("Password", Arrays.toString(Base64.getDecoder().decode(serviceAccountPassword)));

            // Create HttpEntity with headers and body
            HttpEntity<Map<String, String>> entity = new HttpEntity<>(requestBody, headers);

            // Call the API to get the token
            ResponseEntity<Map> response = restTemplate.exchange(v1GenerateTokenUrl, HttpMethod.POST, entity, Map.class);

            log.info("Time taken to fetch token: {} ms", (System.currentTimeMillis() - startTime));

            // Extract the token from the response
            Map<String, Object> responseBody = response.getBody();
            if (responseBody != null && responseBody.containsKey("token")) {
                return (String) responseBody.get("token");
            } else {
                throw new V1ServiceException("Token not found in response");
            }

        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception ex) {
            throw new V1ServiceException("Error fetching token: " + ex.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MASTER_DATA_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MASTER_DATA_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCarrierMasterData(Object request, boolean isListOnly) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            CarrierListObject req = jsonHelper.convertValue(request, CarrierListObject.class);
            Object requestCriteria = req.getListObject();
            HttpEntity<V1DataResponse> entity = new HttpEntity(requestCriteria, V1AuthHelper.getHeaders());
            if (isListOnly ||
                    (req.getListObject() != null && (Objects.equals(Constants.CONSOLIDATION_TYPE_AGT, req.getType()) || Objects.equals(Constants.CONSOLIDATION_TYPE_CLD, req.getType()) )) ||
                    (Objects.equals(Boolean.TRUE, req.getIsList())) ) {
                masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            } else {
                masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_ORG_REF_FILTER_URL, entity, V1DataResponse.class, new Object[0]);
            }
            log.info("Token time taken in getCarrierMasterData() function {} with Request ID: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createCarrierMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createCarrierMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateCarrierMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateCarrierMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchContainerTypeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CONTAINER_TYPE_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getContainerTypeMasterData() function {} with Request ID: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createContainerTypeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CONTAINER_TYPE_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createContainerTypeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateContainerTypeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CONTAINER_TYPE_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateContainerTypeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchVesselData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.VESSEL_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            double _timeTaken = System.currentTimeMillis() - time;
            log.info("Token time taken in getVesselData() function {} with Request ID: {}", _timeTaken, LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createVesselData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.VESSEL_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createVesselData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateVesselData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.VESSEL_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateVesselData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchRoutingMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ROUTING_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getRoutingMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createRoutingMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ROUTING_MASTER_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createRoutingMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateRoutingMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ROUTING_MASTER_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateRoutingMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCurrenciesData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CURRENCIES_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getCurrenciesData() function {} with Request ID: {}", (System.currentTimeMillis() - time), LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createCurrenciesData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CURRENCIES_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createCurrenciesData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateCurrenciesData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CURRENCIES_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateCurrenciesData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchDangerousGoodData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.DANGEROUS_GOOD_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getDangerousGoodData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createDangerousGoodData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.DANGEROUS_GOOD_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createDangerousGoodData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateDangerousGoodData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.DANGEROUS_GOOD_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateDangerousGoodData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchWarehouseData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.WAREHOUSE_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getWarehouseData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createWarehouseData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.WAREHOUSE_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createWarehouseData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateWarehouseData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.WAREHOUSE_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateWarehouseData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchPortsData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.PORTS_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getPortsData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createPortsData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.PORTS_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createPortsData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updatePortsData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.PORTS_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updatePortsData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCommodityData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.COMMODITY_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getCommodityData() function {} with Request ID: {}", (System.currentTimeMillis() - time), LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createCommodityData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.COMMODITY_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createCommodityData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateCommodityData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.COMMODITY_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateCommodityData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchSalesAgentData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SALES_AGENT_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getSalesAgentData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createSalesAgentData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SALES_AGENT_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createSalesAgentData() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateSalesAgentData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SALES_AGENT_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateSalesAgentData() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createOrganizationData(Object request) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            orgResponse = this.restTemplate.postForEntity(this.ORGANIZATION_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createOrganization() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchOrganization(Object request) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            orgResponse = this.restTemplate.postForEntity(this.ORGANIZATION_API, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getOrganization() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchOrganization(Object request, HttpHeaders headers) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, headers);
            orgResponse = this.restTemplate.postForEntity(this.ORGANIZATION_API, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getOrganization() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateOrganizationData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ORGANIZATION_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateOrganizationData() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createUnlocationData(Object request) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            orgResponse = this.restTemplate.postForEntity(this.UNLOCATION_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createUnlocation() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchUnlocation(Object request) {
        ResponseEntity locationResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            locationResponse = this.restTemplate.postForEntity(this.UNLOCATION_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchUnlocation() function {} with Request ID: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) locationResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateUnlocationData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.UNLOCATION_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateUnlocationData() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchMultipleMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MULTIPLE_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchMultipleMasterData() function {} with Request ID: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchUsersData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.USER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchUsersData() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchGridColorCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GRID_COLOR_CODE_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchGridColorCodeData() function {}", (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createGridColorCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GRID_COLOR_CODE_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createGridColorCodeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateGridColorCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GRID_COLOR_CODE_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateGridColorCodeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse listCousinBranches(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_COUSIN_BRANCH_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in listCousinBranches() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse listCousinBranchesWithoutCurrent(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_COUSIN_BRANCH_WITHOUT_CURRENT_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in listCousinBranchesWithoutCurrent() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public TenantIdResponse tenantByGuid(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.TENANT_BY_GUID_URL, entity, TenantIdResponse.class, new Object[0]);
            log.info("Token time taken in tenantByGuid() function " + (System.currentTimeMillis() - time));
            return (TenantIdResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public SendEntityResponse sendConsolidationTask(CreateConsolidationTaskRequest request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SEND_CONSOLIDATION_TASK_URL, entity, SendEntityResponse.class, new Object[0]);
            log.info("Token time taken in sendConsolidationTask() function " + (System.currentTimeMillis() - time));
            return (SendEntityResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public SendEntityResponse sendShipmentTask(CreateShipmentTaskRequest request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SEND_SHIPMENT_TASK_URL, entity, SendEntityResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_SEND_SHIPMENT_TASK_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (SendEntityResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public SendEntityResponse sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SEND_V1_CONSOLIDATION_TASK_URL, entity, SendEntityResponse.class, new Object[0]);
            log.info("Token time taken in sendConsolidationTask() function " + (System.currentTimeMillis() - time));
            return (SendEntityResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public SendEntityResponse sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SEND_V1_SHIPMENT_TASK_URL, entity, SendEntityResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_SEND_SHIPMENT_TASK_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (SendEntityResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }
    @Override
    public CheckTaskExistResponse checkTaskExist(CheckTaskExistV1Request request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CHECK_TASK_EXIST, entity, CheckTaskExistResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_SEND_SHIPMENT_TASK_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (CheckTaskExistResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }


    @Override
    public V1DataResponse importFlightSchedules(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.IMPORT_FLIGHT_SCHEDULE, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in importFlightSchedules() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchFlightStatus(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.FETCH_FLIGHT_STATUS, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchFlightStatus() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse importSailingSchedules(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.IMPORT_SAILING_SCHEDULES, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in importSailingSchedules() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse listSailingSchedule(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_SAILING_SCHEDULE, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in listSailingSchedule() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse addressList(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ADDRESS_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in addressList() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse addressList(Object request, HttpHeaders headers) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, headers);
            masterDataResponse = this.restTemplate.postForEntity(this.ADDRESS_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in addressList() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public List<String> getTenantName(List<Integer> tenantIds) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TENANTID));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(tenantIds)));
        request.setCriteriaRequests(criteria);
        V1DataResponse tenantName = tenantNameByTenantId(request);

        List<V1TenantResponse> v1TenantResponse = jsonHelper.convertValueToList(tenantName.entities, V1TenantResponse.class);
        if (v1TenantResponse != null) {
            return v1TenantResponse.stream().map(V1TenantResponse::getTenantName).toList();
        }
        return Collections.emptyList();
    }

    @Override
    public V1DataResponse tenantNameByTenantId(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.TENANT_NAME_BY_ID, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in tenantNameByTenantId() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }


    @Override
    public V1DataResponse fetchChargeCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CHARGE_TYPE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchChargeCodeData() function {} with Request ID: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1RetrieveResponse retrieveChargeCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.RETRIEVE_CHARGE_TYPE_URL, entity, V1RetrieveResponse.class, new Object[0]);
            log.info("Token time taken in retrieveChargeCodeData() function {} with Request ID: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());
            return (V1RetrieveResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchUnlocationOriginAndDestinationList(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.UNLOCATION_ORIGIN_AND_DESTINATION_LIST_URL, entity, V1DataResponse.class);
            log.info("Token time taken in fetchUnlocationOriginAndDestinationList() function " + (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchListUnlocationTransportModeBased(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_UNLOCATION_TRANSPORT_MODE_BASED_URL, entity, V1DataResponse.class);
            log.info("Token time taken in fetchListUnlocationTransportModeBased() function " + (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchActivityMaster(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ACTIVITY_MASTER_URL, entity, V1DataResponse.class);
            log.info("Token time taken in fetchActivityMaster() function " + (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1RetrieveResponse retrieveTenantSettings() {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.RETRIEVE_TENANT_SETTINGS, entity, V1RetrieveResponse.class, new Object[0]);
            log.info("RequestId: {} || Token time taken in tenantNameByTenantId() function: {} || Response: {}", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - time), jsonHelper.convertToJson(masterDataResponse.getBody()));
            return (V1RetrieveResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public CompanySettingsResponse retrieveCompanySettings() {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.RETRIEVE_COMPANY_SETTINGS, entity, CompanySettingsResponse.class, new Object[0]);
            log.info("Token time taken in retrieveCompanySettings() function " + (System.currentTimeMillis() - time));
            return (CompanySettingsResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1RetrieveResponse retrieveTenant() {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.RETRIEVE_TENANT, entity, V1RetrieveResponse.class, new Object[0]);
            log.info("Token time taken in retrieveTenant() function " + (System.currentTimeMillis() - time));
            return (V1RetrieveResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1RetrieveResponse retrieveTenant(HttpHeaders headers) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(headers);
            masterDataResponse = this.restTemplate.postForEntity(this.RETRIEVE_TENANT, entity, V1RetrieveResponse.class, new Object[0]);
            log.info("Token time taken in retrieveTenant() function " + (System.currentTimeMillis() - time));
            return (V1RetrieveResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public PartyRequestV2 getDefaultOrg() {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GET_DEFAULT_ORG, entity, PartyRequestV2.class, new Object[0]);
            log.info("Token time taken in getDefaultOrg() function " + (System.currentTimeMillis() - time));
            return (PartyRequestV2) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchOwnType(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.OWN_TYPE, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchOwnType() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCarrierFilterList(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_FILTER_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchCarrierFilterList() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchTransportInstructionList(Object request) {
        ResponseEntity tiDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            tiDataResponse = this.restTemplate.postForEntity(this.TRANSPORT_INSTRUCTION_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchTransportInstructionList() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) tiDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchContainersListForTI(Object request) {
        ResponseEntity containerResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            containerResponse = this.restTemplate.postForEntity(this.CONTAINER_TRANSPORT_INSTRUCTION_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchContainersListForTI() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) containerResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public ConsoleBookingListResponse fetchConsolidationBookingData(Object request) {
        ResponseEntity<ConsoleBookingListResponse> consolResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            consolResponse = this.restTemplate.postForEntity(this.CONSOLIDATION_BOOKING_DATA, entity, ConsoleBookingListResponse.class, new Object[0]);
            log.info("Token time taken in fetchConsolidationBookingData() function " + (System.currentTimeMillis() - time));
            return (ConsoleBookingListResponse) consolResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public ShipmentBillingListResponse fetchShipmentBillingData(ShipmentBillingListRequest request) {
        ResponseEntity<ShipmentBillingListResponse> shipmentResponse = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            shipmentResponse = this.restTemplate.postForEntity(this.SHIPMENT_BILLING_DATA, entity, ShipmentBillingListResponse.class);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_SHIPMENT_BILLING_DATA_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return shipmentResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public GuidsListResponse fetchWayBillNumberFilterGuids(Object request) {
        ResponseEntity<GuidsListResponse> shipmentResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            shipmentResponse = this.restTemplate.postForEntity(this.SHIPMENT_WAY_BILL_FILTER, entity, GuidsListResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_SHIPMENT_BILLING_DATA_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (GuidsListResponse) shipmentResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public GuidsListResponse fetchBookingIdFilterGuids(Object request) {
        ResponseEntity<GuidsListResponse> shipmentResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            shipmentResponse = this.restTemplate.postForEntity(this.CONSOLIDATION_BOOKING_ID_FILTER, entity, GuidsListResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_SHIPMENT_BILLING_DATA_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (GuidsListResponse) shipmentResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchGetTemplateMainPage(Object request) {
        ResponseEntity tiDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            tiDataResponse = this.restTemplate.postForEntity(this.MAIN_PAGE_TEMPLATE_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchGetTemplateMainPage() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) tiDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public HblTaskCreationResponse createTaskforHBL(Object request) {
        ResponseEntity tiDataResponse = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            tiDataResponse = this.restTemplate.postForEntity(this.HBL_TASK_CREATION, entity, HblTaskCreationResponse.class, new Object[0]);
            log.info("Total time taken in createTaskforHBL() function " + (System.currentTimeMillis() - time));
            return (HblTaskCreationResponse) tiDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchRolesList(Object request) {
        ResponseEntity tiDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            tiDataResponse = this.restTemplate.postForEntity(this.ROLES_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_ROLES_LIST_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (V1DataResponse) tiDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchBillingList(Object request) {
        ResponseEntity billingDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            billingDataResponse = this.restTemplate.postForEntity(this.BILLING_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_ROLES_LIST_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (V1DataResponse) billingDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchBillChargesList(Object request) {
        ResponseEntity billChargesDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            billChargesDataResponse = this.restTemplate.postForEntity(this.BILL_CHARGES_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_ROLES_LIST_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (V1DataResponse) billChargesDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchArObjectList(Object request) {
        ResponseEntity arObjectDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            arObjectDataResponse = this.restTemplate.postForEntity(this.AR_OBJECT_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info(JOIN_REGEX, TOKEN_TIME_TAKEN_IN_FETCH_ROLES_LIST_FUNCTION_MSG, (System.currentTimeMillis() - time));
            return (V1DataResponse) arObjectDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataSyncResponse v1DataSync(Object request, HttpHeaders headers) {
        ResponseEntity tiDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            if(headers == null)
                headers = v1AuthHelper.getHeadersForDataSync();
            log.info("Request: {} || Payload sent for event: {} with request payload: {} and headers provided are: {}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.V1_DATA_SYNC, jsonHelper.convertToJson(request), headers.toString());
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, headers);
            tiDataResponse = this.restTemplate.postForEntity(this.DATA_SYNC_URL, entity, V1DataSyncResponse.class, new Object[0]);
            log.info("Request: {} || Response for event: {} with response{}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.V1_DATA_SYNC, jsonHelper.convertToJson(tiDataResponse.getBody()));
            log.info("Request: {} || Total time taken in v1DataSync() function: {}", LoggerHelper.getRequestIdFromMDC() ,(System.currentTimeMillis() - time));
            return (V1DataSyncResponse) tiDataResponse.getBody();
        } catch (Exception var7) {
            return V1DataSyncResponse.builder().error(var7.getMessage()).isSuccess(false).build();
        }
    }

    @Override
    public String getMaxShipmentId() {
        ResponseEntity v1Response = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(V1AuthHelper.getHeaders());
            v1Response = this.restTemplate.exchange(this.GET_MAX_SHIPMENT_ID_URL, HttpMethod.GET, entity, Object.class);
            log.info("{} {} {}", REQUEST_TOTAL_TIME_TAKEN_TO_GET_MAX_SHIPMENT_ID_MSG, LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - time));
            return (String) v1Response.getBody();
        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public String getShipmentSerialNumber() {
        ResponseEntity v1Response = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(V1AuthHelper.getHeaders());
            v1Response = this.restTemplate.exchange(this.GET_SHIPMENT_SEQUENCE_NUMBER_URL, HttpMethod.GET, entity, Object.class);
            log.info(REQUEST_TOTAL_TIME_TAKEN_TO_GET_MAX_SHIPMENT_ID_MSG, LoggerHelper.getRequestIdFromMDC() ,(System.currentTimeMillis() - time));
            return (String) v1Response.getBody();
        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public String getMaxConsolidationId() {
        ResponseEntity v1Response = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(V1AuthHelper.getHeaders());
            v1Response = this.restTemplate.exchange(this.GET_MAX_CONSOL_ID_URL, HttpMethod.GET, entity, Object.class);
            log.info(REQUEST_TOTAL_TIME_TAKEN_TO_GET_MAX_SHIPMENT_ID_MSG, LoggerHelper.getRequestIdFromMDC() ,(System.currentTimeMillis() - time));
            return (String) v1Response.getBody();
        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1RetrieveResponse getShipment(V1RetrieveRequest request) {
        ResponseEntity masterDataResponse = null;
        try {
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SHIPMENT_RETRIEVE_URL, entity, V1RetrieveResponse.class, new Object[0]);
            return (V1RetrieveResponse) masterDataResponse.getBody();

        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }
    @Override
    public CreditLimitValidateResponse checkCreditLimit(CreditLimitValidateRequest request) {
        ResponseEntity masterDataResponse = null;
        try {
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CREDIT_LIMIT_CHECK_URL, entity, CreditLimitValidateResponse.class, new Object[0]);
            return (CreditLimitValidateResponse) masterDataResponse.getBody();
        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else if(var6.getStatusCode() == HttpStatus.BAD_REQUEST){
                throw new V1ServiceException(jsonHelper.readFromJson(var6.getResponseBodyAsString(), CreditLimitValidateResponse.class).getError().getMessage());
            } else {
                throw var6;
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public AddressTranslationListResponse getAddressTranslation(AddressTranslationRequest request) {
        ResponseEntity masterDataResponse = null;
        try {
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GET_ADDRESS_TRANSLATION, entity, AddressTranslationListResponse.class, new Object[0]);
            return (AddressTranslationListResponse) masterDataResponse.getBody();

        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public CheckActiveInvoiceResponse getActiveInvoices(CheckActiveInvoiceRequest request) {
        ResponseEntity masterDataResponse = null;
        try {
            if(Objects.equals(commonUtils.getShipmentSettingFromContext().getShipmentLite(), false))
            {
                return CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(false).build();
            }
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GET_ACTIVE_INVOICES, entity, CheckActiveInvoiceResponse.class, new Object[0]);
            return (CheckActiveInvoiceResponse) masterDataResponse.getBody();

        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCreditLimit(Object request) {
        ResponseEntity creditLimitResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            creditLimitResponse = this.restTemplate.postForEntity(this.CREDIT_LIMIT_LIST, entity, V1DataResponse.class);
            log.info("Token time taken in fetchCreditLimit() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) creditLimitResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }
    @Override
    public OrgAddressResponse fetchOrgAddresses(Object request) {
        ResponseEntity response = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            response = this.restTemplate.postForEntity(this.ORG_ADDRESS_LIST, entity, OrgAddressResponse.class);
            log.info("Token time taken in fetchOrgAddresses() function " + (System.currentTimeMillis() - time));
            return (OrgAddressResponse) response.getBody();
        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public EntityTransferAddress fetchAddress(String addressId) {
        try {
            V1RetrieveRequest retrieveRequest = V1RetrieveRequest.builder().EntityId(addressId).build();
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(retrieveRequest, V1AuthHelper.getHeaders());
            ResponseEntity responseEntity = this.restTemplate.postForEntity(this.ADDRESS_RETRIEVE, entity, V1RetrieveResponse.class, new Object[0]);
            log.info("Total time taken in fetchAddress() function: {}", (System.currentTimeMillis() - time));
            return modelMapper.map(responseEntity.getBody(), EntityTransferAddress.class);
        } catch (HttpClientErrorException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse getCoLoadingStations(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GET_CO_LOAD_STATIONS, entity, V1DataResponse.class);
            long elapsedTime = System.currentTimeMillis() - time;
            if(log.isInfoEnabled()) {
                log.info(String.format("Token time taken in getColoadingStations() function: %d ms", elapsedTime));
            }
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public TenantDetailsByListResponse getTenantDetails(Object request) {
        ResponseEntity<TenantDetailsByListResponse> responseEntity = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            responseEntity = this.restTemplate.postForEntity(this.getTenantInfoUrl, entity, TenantDetailsByListResponse.class);
            log.info("Token time taken in getTenantDetails() function {} ms", System.currentTimeMillis() - time);
            return responseEntity.getBody();
            } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse getEmailTemplates(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
             masterDataResponse = this.restTemplate.postForEntity(this.getEmailTemplates, entity, V1DataResponse.class, V1DataResponse.class);
            log.info("Token time taken in getEmailTemplates() function {} ms", (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse getMasterDetails(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.getMasterDetails, entity, V1DataResponse.class, V1DataResponse.class);
            log.info("Token time taken in getMasterDetails() function {} ms", (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse getUserDetails(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.getUserDetails, entity, V1DataResponse.class, V1DataResponse.class);
            log.info("Token time taken in getUserDetails() function {} ms", (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public TaskCreateResponse createTask(Object request) {
        ResponseEntity<TaskCreateResponse> response;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            response = this.restTemplate.postForEntity(this.createTaskUrl, entity, TaskCreateResponse.class);
            log.info("Token time taken in createTask() function {}", (System.currentTimeMillis() - time));
            return response.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateTask(Object request) {
        ResponseEntity<V1DataResponse> response;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            response = this.restTemplate.postForEntity(this.updateTaskUrl, entity, V1DataResponse.class);
            log.info("Token time taken in updateTask() function {}", (System.currentTimeMillis() - time));
            return response.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1RetrieveResponse retrieveTask(Object request) {
        ResponseEntity<V1RetrieveResponse> response;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            response = this.restTemplate.postForEntity(this.retrieveTaskUrl, entity, V1RetrieveResponse.class);
            log.info("Token time taken in retrieveTask() function {}", (System.currentTimeMillis() - time));
            return response.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }
    @Override
    public V1DataResponse listTask(Object request) {
        ResponseEntity<V1DataResponse> response;
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(jsonHelper.convertToJson(request), V1AuthHelper.getHeaders());
            response = this.restTemplate.postForEntity(this.listTaskUrl, entity, V1DataResponse.class);
            log.info("Token time taken in retrieveTask() function {}", (System.currentTimeMillis() - time));
            return response.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }


    @Override
    public List<UsersRoleListResponse> getUserEmailsByRoleId(V1UsersEmailRequest request) {
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            ResponseEntity<List<UsersRoleListResponse>> response = this.restTemplate.exchange(
                    this.getUserEmailsByRoleId,
                    HttpMethod.POST,
                    entity,
                    new ParameterizedTypeReference<List<UsersRoleListResponse>>() {}
            );

            log.info("Token time taken in getUserEmailsByRoleId() function {} ms", (System.currentTimeMillis() - time));
            return response.getBody();
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            throw new V1ServiceException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public Integer getRoleIdsByRoleName(V1RoleIdRequest request) {
        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            ResponseEntity<V1RoleIdResponse> response = this.restTemplate.exchange(
                this.getRolesIdByRoleName,
                HttpMethod.POST,
                entity,
                new ParameterizedTypeReference<V1RoleIdResponse>() {}
            );
            log.info("Token time taken in getRoleIdsByRoleName() function {}", (System.currentTimeMillis() - time));
            return Objects.requireNonNull(response.getBody()).getRoleId();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            } else {
                throw new V1ServiceException(jsonHelper.readFromJson(var6.getResponseBodyAsString(), V1ErrorResponse.class).getError().getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }
}
