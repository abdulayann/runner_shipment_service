package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
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

    ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel);

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException;

    CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;

    ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws Exception;

    ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel);

    ResponseEntity<?> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map, List<NotesRequest> customerBookingNotes) throws Exception;

    ResponseEntity<?> cloneShipment(CommonRequestModel commonRequestModel);

    ResponseEntity<?> transportInstructionList(CommonRequestModel commonRequestModel);

    ResponseEntity<?> containerListForTI(CommonRequestModel commonRequestModel);

    void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, boolean isCreate);

    ResponseEntity<?> fullShipmentsList(CommonRequestModel commonRequestModel);

    ResponseEntity<?> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws Exception;

    ResponseEntity<?> assignShipmentContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<?> assignAllContainers(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveByOrderId(String orderId);

    ResponseEntity<?> generateCustomHouseBLNumber();

    ResponseEntity<?> getShipmentFromConsol(Long consolidationId, String bookingNumber);

    ResponseEntity<?> getDefaultShipment();

    ResponseEntity<?> attachListShipment(CommonRequestModel commonRequestModel);

    ResponseEntity<?> getMasterDataMappings();

    ResponseEntity<?> calculateContainerSummary(CommonRequestModel commonRequestModel) throws Exception;
    
    ResponseEntity<?> calculatePackSummary(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> getAllMasterData(CommonRequestModel commonRequestModel);
    String generateCustomHouseBL(ShipmentDetails shipmentDetails);
    ResponseEntity<?> getIdFromGuid(CommonRequestModel commonRequestModel);
    ResponseEntity<?> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel);
    ResponseEntity<?> fetchActiveInvoices(CommonRequestModel commonRequestModel);
    ResponseEntity<?> showAssignAllContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<?> fetchCreditLimit(String orgCode, String addressCode);
    void updateDateAndStatus(long id, LocalDateTime date, Integer status);
    ResponseEntity<?> fetchEmails(Long shipmentId, Long consolidationId);
    ResponseEntity<?> getGuidFromId(CommonRequestModel commonRequestModel);

}