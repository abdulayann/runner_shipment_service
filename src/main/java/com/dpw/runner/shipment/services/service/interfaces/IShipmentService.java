package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.UpdateConsoleShipmentRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.billing.InvoicePostingValidationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.request.PartiesOrgAddressRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import org.apache.http.auth.AuthenticationException;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public interface IShipmentService extends ICommonService {
    List<ShipmentDetails> createTestShipment(Integer count) throws RunnerException;

    ResponseEntity<IRunnerResponse> fetchShipments(CommonRequestModel commonRequestModel);

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException, ExecutionException, InterruptedException;

    CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;

    ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;
    ShipmentDetailsResponse completeUpdateShipmentFromEntityTransfer(ShipmentRequest shipmentRequest) throws RunnerException;

    ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException;

    ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> syncShipmentAuditLogsToService(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map, List<NotesRequest> customerBookingNotes, boolean dataMigration, List<AuditLogRequestV2> auditLogRequestV2, String createdBy) throws RunnerException;

    Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse);

    ResponseEntity<IRunnerResponse> cloneShipment(CommonRequestModel commonRequestModel);
    ShipmentDetailsResponse createShipmentFromEntityTransfer(ShipmentRequest shipmentRequest);

    ResponseEntity<IRunnerResponse> transportInstructionList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> containerListForTI(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fullShipmentsList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fullShipmentsExternalList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> fetchBillChargesShipmentList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException;

    String createShipmentFromBooking(ShipmentRequest shipmentRequest) throws RunnerException;

    ResponseEntity<IRunnerResponse> assignShipmentContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> assignAllContainers(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId) throws RunnerException;

    ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException;

    ResponseEntity<IRunnerResponse> getShipmentFromConsol(Long consolidationId, String bookingNumber);

    ResponseEntity<IRunnerResponse> getDefaultShipment();

    ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getMasterDataMappings();

    ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException;
    
    ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws RunnerException;
    List<RoutingsRequest> getCustomerBookingRequestRoutingList(CarrierDetailRequest carrierDetailRequest, String transportMode);
    ResponseEntity<IRunnerResponse> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel);
    String generateCustomHouseBL(ShipmentDetails shipmentDetails);
    ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> fetchActiveInvoices(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> showAssignAllContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> fetchCreditLimit(String orgCode, String addressCode) throws RunnerException;
    void updateDateAndStatus(Long id, LocalDateTime date, Integer status) throws RunnerException;
    ResponseEntity<IRunnerResponse> fetchEmails(Long shipmentId, Long consolidationId);
    ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> checkCreditLimitFromV1(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getDateTimeChangeUpdates(Long shipmentId) throws RunnerException;
    void validateRaKcDetails(ShipmentDetails shipmentDetails, V1TenantSettingsResponse tenantSettingsResponse) throws RunnerException;
    ResponseEntity<IRunnerResponse> consoleShipmentList(CommonRequestModel commonRequestModel, Long consoleId, String consoleGuid, boolean isAttached, boolean getMasterData, boolean fromNte) throws AuthenticationException;
    ResponseEntity<IRunnerResponse> shipmentRetrieveWithMeasurmentBasis(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getAllShipments(Long consoleId);
    ResponseEntity<IRunnerResponse> getLatestCargoDeliveryDate(Long consoleId);

    ResponseEntity<IRunnerResponse> updateShipments(UpdateConsoleShipmentRequest request) throws RunnerException;
    ResponseEntity<IRunnerResponse> requestInterBranchConsole(Long shipId, Long consoleId, String remarks) throws RunnerException;

    ResponseEntity<IRunnerResponse> getContainerListFromTrackingService(Long shipmentId, Long consolidationId) throws RunnerException;

    ResponseEntity<IRunnerResponse> validateInvoicePosting(InvoicePostingValidationRequest request);

    ResponseEntity<IRunnerResponse> getPendingNotifications(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> attachDetachOrder(ShipmentOrderAttachDetachRequest shipmentRequest);

    ResponseEntity<IRunnerResponse> calculateShipmentSummary(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> sendOceanDGApprovalEmail(OceanDGApprovalRequest dgApprovalRequest) throws RunnerException;

    ResponseEntity<IRunnerResponse> dgApprovalResponse(OceanDGRequest request) throws RunnerException;
    PartiesRequest fetchOrgInfoFromV1(PartiesOrgAddressRequest request) throws RunnerException;

    ResponseEntity<IRunnerResponse> hblCheck(String hblNumber, String shipmentId);
    ResponseEntity<IRunnerResponse> listWithoutTenantCheck(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> cancel(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData);
    ResponseEntity<IRunnerResponse> retrieveForNTE(CommonRequestModel commonRequestModel);
     void triggerAutomaticTransfer(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isDocAdded);

}