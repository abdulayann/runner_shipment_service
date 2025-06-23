package com.dpw.runner.shipment.services.service.v1;

import com.dpw.runner.shipment.services.dto.request.UserWithPermissionRequestV1;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.List;
import java.util.UUID;

public interface IV1Service {
    ResponseEntity<V1ShipmentCreationResponse> createBooking(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled, UUID shipmentGuid, HttpHeaders headers);
    ResponseEntity<UpdateOrgCreditLimitBookingResponse> updateOrgCreditLimitFromBooking(CheckCreditLimitResponse request);

    void setAuthContext();

    void setAuthContext(String token, UsersDto user);

    List<SimpleGrantedAuthority> getAuthorities(List<String> permissions);

    void clearAuthContext();

    String generateToken();

    V1DataResponse fetchMasterData(Object request);

    V1DataResponse createMasterData(Object request);

    V1DataResponse updateMasterData(Object request);

    V1DataResponse fetchCarrierMasterData(Object request, boolean isListOnly);

    V1DataResponse createCarrierMasterData(Object request);

    V1DataResponse updateCarrierMasterData(Object request);

    V1DataResponse fetchOrganization(Object request);

    V1DataResponse fetchUnlocation(Object request);
    V1DataResponse stateBasedList(Object request);


    V1DataResponse fetchContainerTypeData(Object request);

    V1DataResponse createContainerTypeData(Object request);

    V1DataResponse updateContainerTypeData(Object request);

    V1DataResponse fetchVesselData(Object request);

    V1DataResponse createVesselData(Object request);

    V1DataResponse updateVesselData(Object request);

    V1DataResponse fetchRoutingMasterData(Object request);

    V1DataResponse createRoutingMasterData(Object request);

    V1DataResponse updateRoutingMasterData(Object request);

    V1DataResponse fetchCurrenciesData(Object request);

    V1DataResponse createCurrenciesData(Object request);

    V1DataResponse updateCurrenciesData(Object request);

    V1DataResponse fetchDangerousGoodData(Object request);

    V1DataResponse createDangerousGoodData(Object request);

    V1DataResponse updateDangerousGoodData(Object request);

    V1DataResponse fetchWarehouseData(Object request);

    V1DataResponse createWarehouseData(Object request);

    V1DataResponse updateWarehouseData(Object request);

    V1DataResponse fetchPortsData(Object request);

    V1DataResponse createPortsData(Object request);

    V1DataResponse updatePortsData(Object request);

    V1DataResponse fetchCommodityData(Object request);

    V1DataResponse createCommodityData(Object request);

    V1DataResponse updateCommodityData(Object request);

    V1DataResponse fetchSalesAgentData(Object request);

    V1DataResponse createSalesAgentData(Object request);

    V1DataResponse updateSalesAgentData(Object request);

    V1DataResponse createOrganizationData(Object request);

    V1DataResponse fetchOrganization(Object request, HttpHeaders httpHeaders);

    V1DataResponse updateOrganizationData(Object request);

    V1DataResponse createUnlocationData(Object request);

    V1DataResponse updateUnlocationData(Object request);

    V1DataResponse fetchMultipleMasterData(Object request);

    V1DataResponse fetchUsersData(Object request);

    V1DataResponse fetchGridColorCodeData(Object request);

    V1DataResponse createGridColorCodeData(Object request);

    V1DataResponse updateGridColorCodeData(Object request);

    V1DataResponse listCousinBranches(Object request);

    V1DataResponse listCousinBranchesWithoutCurrent(Object request);

    TenantIdResponse tenantByGuid(Object request);

    V1DataResponse importFlightSchedules(Object request);

    V1DataResponse fetchFlightStatus(Object request);

    V1DataResponse importSailingSchedules(Object request);

    V1DataResponse listSailingSchedule(Object request);

    SendEntityResponse sendConsolidationTask(CreateConsolidationTaskRequest request);

    SendEntityResponse sendShipmentTask(CreateShipmentTaskRequest request);
    SendEntityResponse sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request request);
    SendEntityResponse sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request request);
    CheckTaskExistResponse checkTaskExist(CheckTaskExistV1Request request);

    V1DataResponse addressList(Object request);

    V1DataResponse addressList(Object request, HttpHeaders headers);

    List<String> getTenantName(List<Integer> tenantIds);
    V1DataResponse tenantNameByTenantId(Object request);
    V1DataResponse fetchChargeCodeData(Object request);
    V1RetrieveResponse retrieveChargeCodeData(Object request);
    V1DataResponse fetchUnlocationOriginAndDestinationList(Object request);
    V1DataResponse fetchListUnlocationTransportModeBased(Object request);
    V1DataResponse fetchActivityMaster(Object request);
    V1RetrieveResponse retrieveTenantSettings();
    CompanySettingsResponse retrieveCompanySettings();
    V1RetrieveResponse retrieveTenant();
    V1RetrieveResponse retrieveTenant(HttpHeaders headers);
    PartyRequestV2 getDefaultOrg();

    V1DataResponse fetchOwnType(Object request);

    V1DataResponse fetchCarrierFilterList(Object request);
    V1DataResponse fetchTransportInstructionList(Object request);
    V1DataResponse fetchContainersListForTI(Object request);
    ConsoleBookingListResponse fetchConsolidationBookingData(Object request);
    GuidsListResponse fetchWayBillNumberFilterGuids(Object request);
    GuidsListResponse fetchBookingIdFilterGuids(Object request);
    V1DataResponse fetchGetTemplateMainPage(Object request);
    HblTaskCreationResponse createTaskforHBL(Object request);
    ShipmentBillingListResponse fetchShipmentBillingData(ShipmentBillingListRequest request);
    V1DataResponse fetchRolesList(Object request);
    V1DataResponse fetchBillingList(Object request);
    V1DataResponse fetchBillChargesList(Object request);
    V1DataResponse fetchArObjectList(Object request);
    V1DataSyncResponse v1DataSync(Object request, HttpHeaders headers);

    String getMaxShipmentId();
    String getShipmentSerialNumber();
    String getMaxConsolidationId();
    V1RetrieveResponse getShipment(V1RetrieveRequest request);
    CreditLimitValidateResponse checkCreditLimit(CreditLimitValidateRequest request);
    AddressTranslationListResponse getAddressTranslation(AddressTranslationRequest request);
    CheckActiveInvoiceResponse getActiveInvoices(CheckActiveInvoiceRequest request);
    V1DataResponse fetchCreditLimit(Object request);
    OrgAddressResponse fetchOrgAddresses(Object request);
    EntityTransferAddress fetchAddress(String entityId);
    V1DataResponse getCoLoadingStations(Object request);
    TenantDetailsByListResponse getTenantDetails(Object request);
    V1DataResponse getEmailTemplates(Object request);
    V1DataResponse getEmailTemplatesWithTenantId(Object request);
    V1DataResponse getMasterDetails(Object request);
    V1DataResponse getUserDetails(Object request);
    TaskCreateResponse createTask(Object request);
    V1DataResponse updateTask(Object request);
    V1RetrieveResponse retrieveTask(Object request);
    V1DataResponse listTask(Object request);
    List<UsersRoleListResponse> getUserEmailsByRoleId(V1UsersEmailRequest request);

    Integer getRoleIdsByRoleName(V1RoleIdRequest roleIdRequest);

    V1DataResponse getCompaniesDetails(Object request);

    V1DataResponse listOrgs(Object request);
    V1DataResponse listBranchesByDefaultOrgAndAddress(Object request);
    List<UsersDto> getUsersWithGivenPermissions(UserWithPermissionRequestV1 request);

    V1DataResponse fetchActiveUnlocation(Object request);
}
