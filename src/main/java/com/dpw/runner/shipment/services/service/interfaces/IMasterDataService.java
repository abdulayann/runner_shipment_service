package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IMasterDataService extends ICommonService{
    ResponseEntity<?> createCarrier(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateCarrier(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listCarrier(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createContainerType(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateContainerType(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listContainerType(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createVessel(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateVessel(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listVessel(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createRoutingMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateRoutingMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listRoutingMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createCurrencies(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateCurrencies(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listCurrencies(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createDangerousGood(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateDangerousGood(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listDangerousGood(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createWarehouse(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateWarehouse(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listWarehouse(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createPorts(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updatePorts(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listPorts(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createCommodity(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateCommodity(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listCommodity(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createSalesAgent(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateSalesAgent(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listSalesAgent(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createOrganization(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateOrganization(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listOrganization(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createUnlocation(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateUnlocation(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listUnlocation(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listUsers(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createGridColorCode(CommonRequestModel commonRequestModel);

    ResponseEntity<?> updateGridColorCode(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listGridColorCode(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listCousinBranches(CommonRequestModel commonRequestModel);
    ResponseEntity<?> listCousinBranchesWithoutCurrent(CommonRequestModel commonRequestModel);

    ResponseEntity<?> importFlightSchedules(CommonRequestModel commonRequestModel);

    ResponseEntity<?> fetchFlightStatus(CommonRequestModel commonRequestModel);

    ResponseEntity<?> importSailingSchedules(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listSailingSchedule(CommonRequestModel commonRequestModel);

    ResponseEntity<?> addressList(CommonRequestModel commonRequestModel);

    ResponseEntity<?> tenantNameByTenantId(CommonRequestModel commonRequestModel);

    ResponseEntity<?> fetchUnlocationOriginAndDestinationList(CommonRequestModel commonRequestModel);

    ResponseEntity<?> fetchListUnlocationTransportModeBased(CommonRequestModel commonRequestModel);

    ResponseEntity<?> fetchActivityMaster(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveTenantSettings();

    ResponseEntity<?> retrieveTenant();

    ResponseEntity<?> listOwnType(CommonRequestModel commonRequestModel);

    ResponseEntity<?> listCarrierFilter(CommonRequestModel commonRequestModel);
    ResponseEntity<?> fetchGetTemplateMainPage(CommonRequestModel commonRequestModel);
}
