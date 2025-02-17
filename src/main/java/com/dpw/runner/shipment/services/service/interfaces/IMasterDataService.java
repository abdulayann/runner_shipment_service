package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ListCousinBranchesForEtRequest;
import org.springframework.http.ResponseEntity;

public interface IMasterDataService extends ICommonService {
    ResponseEntity<IRunnerResponse> createCarrier(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateCarrier(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCarrier(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createContainerType(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateContainerType(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listContainerType(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createVessel(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateVessel(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listVessel(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createRoutingMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateRoutingMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listRoutingMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createCurrencies(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateCurrencies(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCurrencies(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createDangerousGood(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateDangerousGood(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listDangerousGood(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createWarehouse(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateWarehouse(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listWarehouse(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createPorts(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updatePorts(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listPorts(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createCommodity(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateCommodity(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCommodity(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createSalesAgent(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateSalesAgent(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listSalesAgent(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createOrganization(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateOrganization(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listOrganization(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createUnlocation(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateUnlocation(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listUnlocation(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> stateBasedList(CommonRequestModel commonRequestModel);


    ResponseEntity<IRunnerResponse> listUsers(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createGridColorCode(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateGridColorCode(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listGridColorCode(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCousinBranches(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCousinBranchesWithoutCurrent(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> importFlightSchedules(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchFlightStatus(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> importSailingSchedules(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listSailingSchedule(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> addressList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> tenantNameByTenantId(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchUnlocationOriginAndDestinationList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchListUnlocationTransportModeBased(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchActivityMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveTenantSettings();

    ResponseEntity<IRunnerResponse> retrieveTenant();

    ResponseEntity<IRunnerResponse> listOwnType(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCarrierFilter(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchGetTemplateMainPage(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listRoles(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchChargeTypes(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getDefaultOrg(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listOrgs(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchMultipleMasterData(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listBranchesByDefaultOrgAndAddress(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCousinBranchForEt(ListCousinBranchesForEtRequest request);
}
