package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
    List<ShipmentDetails> createTestShipment(Integer count);

    ResponseEntity<IRunnerResponse> fetchShipments(CommonRequestModel commonRequestModel);

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException;

    CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;

    ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException;

    ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map, List<NotesRequest> customerBookingNotes) throws RunnerException;

    ResponseEntity<IRunnerResponse> cloneShipment(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> transportInstructionList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> containerListForTI(CommonRequestModel commonRequestModel);

    void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, boolean isCreate);

    ResponseEntity<IRunnerResponse> fullShipmentsList(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException;

    ResponseEntity<IRunnerResponse> assignShipmentContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> assignAllContainers(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId);

    ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber();

    ResponseEntity<IRunnerResponse> getShipmentFromConsol(Long consolidationId, String bookingNumber);

    ResponseEntity<IRunnerResponse> getDefaultShipment();

    ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getMasterDataMappings();

    ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException;
    
    ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel);
    String generateCustomHouseBL(ShipmentDetails shipmentDetails);
    ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> fetchActiveInvoices(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> showAssignAllContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> fetchCreditLimit(String orgCode, String addressCode);
    void updateDateAndStatus(long id, LocalDateTime date, Integer status);
    ResponseEntity<IRunnerResponse> fetchEmails(Long shipmentId, Long consolidationId);
    ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel);

}