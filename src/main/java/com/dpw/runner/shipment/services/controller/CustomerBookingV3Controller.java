package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CustomerBookingConstants.CUSTOMER_BOOKING_API_HANDLE_V3)
public class CustomerBookingV3Controller {

    private static class ReferenceListResponseClass extends RunnerListResponse<ReferenceNumbersResponse> {}
    private static class PartiesListResponseClass extends RunnerListResponse<PartiesResponse> {}

    private final IContainerV3Service containerV3Service;
    private final IPackingV3Service packingV3Service;
    private final IReferenceNumbersV3Service referenceNumbersV3Service;
    private final IPartiesV3Service partiesV3Service;
    private final ICustomerBookingV3Service customerBookingV3Service;
    private final ICRPServiceAdapter crpService;
    private final JsonHelper jsonHelper;

    @Autowired
    public CustomerBookingV3Controller(IPackingV3Service packingV3Service,
                                       IContainerV3Service containerV3Service,
                                       IReferenceNumbersV3Service referenceNumbersV3Service,
                                       IPartiesV3Service partiesV3Service,
                                       ICustomerBookingV3Service customerBookingV3Service,
                                       ICRPServiceAdapter crpService,
                                       JsonHelper jsonHelper) {
        this.containerV3Service = containerV3Service;
        this.packingV3Service = packingV3Service;
        this.referenceNumbersV3Service = referenceNumbersV3Service;
        this.partiesV3Service = partiesV3Service;
        this.customerBookingV3Service = customerBookingV3Service;
        this.crpService = crpService;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CustomerBookingConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_CREATE + "')")
    public ResponseEntity<IRunnerResponse> createBooking(@RequestBody @Valid CustomerBookingV3Request request, @RequestParam(defaultValue = "false") @Valid Boolean isSaveAsDraft) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.create(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class)))})
    @PutMapping(ApiConstants.API_UPDATE_BOOKING)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> updateBooking(@RequestBody @Valid CustomerBookingV3Request request, @RequestParam(defaultValue = "false") @Valid Boolean isSaveAsDraft) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.update(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3DeleteResponse.class)))})
    @DeleteMapping(ApiConstants.API_DELETE)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> deleteBooking(@RequestParam @Valid Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.delete(id));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = CustomerBookingV3ListResponse.class))), description = CustomerBookingConstants.LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) throws RunnerException {
        CustomerBookingV3ListResponse customerBookingV3ListResponse = customerBookingV3Service.list(listCommonRequest, getMasterData);
        return ResponseHelper.buildListSuccessBookingResponse(customerBookingV3ListResponse.getCustomerBookingV3Responses(), customerBookingV3ListResponse.getTotalPages(), customerBookingV3ListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class, description = CustomerBookingConstants.RETRIEVE_BY_ID_SUCCESSFUL)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = CustomerBookingConstants.BOOKING_ID) @RequestParam Optional<Long> id, @Parameter(description = CustomerBookingConstants.BOOKING_GUID) @RequestParam Optional<String> guid) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.retrieveById(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class, description = CustomerBookingConstants.RETRIEVE_BY_ID_SUCCESSFUL)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_BOOKING_NUMBER)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@RequestParam String bookingNumber) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.findByBookingNumber(bookingNumber));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class)))})
    @PutMapping(ApiConstants.API_CANCEL_BOOKING)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_CANCEL + "')")
    public ResponseEntity<IRunnerResponse> cancel(@RequestBody @Valid CustomerBookingV3Request request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.update(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class)))})
    @GetMapping(ApiConstants.API_CLONE)
    public ResponseEntity<IRunnerResponse> cloneById(@Parameter(description = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.cloneBooking(id));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.RETRIEVE_BY_ORDER_ID_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ORDER_ID)
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(@Parameter(description = CustomerBookingConstants.ORDER_ID, required = true) @RequestParam String orderId) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.retrieveByOrderId(orderId));
    }

    @PostMapping(CustomerBookingConstants.PLATFORM_CREATE_BOOKING)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CustomerBookingConstants.CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = PlatformToRunnerCustomerBookingResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> platformCreateBooking(@RequestBody @Valid PlatformToRunnerCustomerBookingRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.platformCreateBooking(request));
    }

    @PostMapping(CustomerBookingConstants.CRP_LIST)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CustomerBookingConstants.LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> listCRPService(@RequestBody @Valid CRPListRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(crpService.listCRPService(CommonRequestModel.buildRequest(request)));
    }

    @PostMapping(CustomerBookingConstants.CRP_RETRIEVE)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = CustomerBookingConstants.RETRIEVE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> retrieveCRPService(@RequestBody @Valid CRPRetrieveRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(crpService.retrieveCRPService(CommonRequestModel.buildRequest(request)));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CheckCreditLimitResponse.class)))})
    @PostMapping(CustomerBookingConstants.FUSION_CHECK_CREDIT_LIMIT)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromFusion(@RequestBody @Valid CreditLimitRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.checkCreditLimitFromFusion(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = V1ShipmentCreationResponse.class)))})
    @GetMapping(CustomerBookingConstants.RETRY_FOR_BILLING)
    public ResponseEntity<IRunnerResponse> retryForBilling(@Parameter(description = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.retryForBilling(CommonGetRequest.builder().id(id).build()));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ContainerResponse.class)))})
    @PostMapping(ApiConstants.BOOKING_API_CREATE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> createBookingContainers(@RequestBody @Valid BookingContainerV3Request bookingContainerV3Request) throws RunnerException {
        ContainerResponse containerResponse = containerV3Service.create(jsonHelper.convertValue(bookingContainerV3Request, ContainerV3Request.class), BOOKING);
        return ResponseHelper.buildSuccessResponse(containerResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = BulkContainerResponse.class)))})
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> updateBookingContainers(@RequestBody @Valid List<BookingContainerV3Request> bookingContainerV3Requests) throws RunnerException {
        BulkContainerResponse containerResponse = containerV3Service.updateBulk(jsonHelper.convertValueToList(bookingContainerV3Requests, ContainerV3Request.class), BOOKING);
        return ResponseHelper.buildSuccessResponse(containerResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ContainerListResponse.class)))})
    @PostMapping(ApiConstants.BOOKING_API_LIST_CONTAINERS)
    public ResponseEntity<IRunnerResponse> listBookingContainers(@RequestBody ListCommonRequest listCommonRequest) throws RunnerException {
        ContainerListResponse containerListResponse = containerV3Service.list(listCommonRequest, true, null);
        return ResponseHelper.buildSuccessResponse(containerListResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ContainerConstants.CONTAINER_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = BulkContainerResponse.class)))})
    @DeleteMapping(ApiConstants.BOOKING_API_DELETE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> deleteBookingContainers(@RequestBody @Valid List<BookingContainerV3Request> bookingContainerV3Requests) throws RunnerException {
        BulkContainerResponse bulkContainerResponse = containerV3Service.deleteBulk(jsonHelper.convertValueToList(bookingContainerV3Requests, ContainerV3Request.class), BOOKING);
        return ResponseHelper.buildSuccessResponse(bulkContainerResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PackingConstants.PACKING_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = PackingResponse.class)))})
    @PostMapping(ApiConstants.BOOKING_API_CREATE_PACKAGES)
    public ResponseEntity<IRunnerResponse> createBookingPackages(@RequestBody @Valid PackingV3Request packingV3Request) throws RunnerException {
        PackingResponse packingResponse = packingV3Service.create(packingV3Request, BOOKING);
        return ResponseHelper.buildSuccessResponse(packingResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PackingConstants.PACKING_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = BulkPackingResponse.class)))})
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_PACKAGES)
    public ResponseEntity<IRunnerResponse> updateBookingPackages(@RequestBody @Valid List<PackingV3Request> packingV3RequestList) throws RunnerException {
        BulkPackingResponse bulkPackingResponse = packingV3Service.updateBulk(packingV3RequestList, BOOKING, false);
        return ResponseHelper.buildSuccessResponse(bulkPackingResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PackingConstants.PACKING_LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation = PackingListResponse.class)))})
    @PostMapping(ApiConstants.BOOKING_API_LIST_PACKAGES)
    public ResponseEntity<IRunnerResponse> listBookingPackages(@RequestBody ListCommonRequest listCommonRequest) {
        PackingListResponse packingListResponse = packingV3Service.list(listCommonRequest, true, null, BOOKING);
        return ResponseHelper.buildSuccessResponse(packingListResponse);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PackingConstants.PACKING_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = BulkPackingResponse.class)))})
    @DeleteMapping(ApiConstants.BOOKING_API_DELETE_PACKAGES)
    public ResponseEntity<IRunnerResponse> deleteBookingPackages(@RequestBody @Valid List<PackingV3Request> packingV3RequestList) throws RunnerException {
        BulkPackingResponse response = packingV3Service.deleteBulk(packingV3RequestList, BOOKING);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @PostMapping(ApiConstants.BOOKING_API_CREATE_REFERENCES)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ReferenceNumbersResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> createBookingReferences(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.create(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ReferenceNumbersResponse.class)))})
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_REFERENCES)
    public ResponseEntity<IRunnerResponse> updateBookingReferences(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.update(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ReferenceNumbersConstants.REFERENCE_NUMBERS_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ReferenceNumbersResponse.class)))})
    @PostMapping(ApiConstants.BOOKING_API_DELETE_REFERENCES)
    public ResponseEntity<IRunnerResponse> deleteBookingReferences(@RequestBody @Valid ReferenceNumbersRequest request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.delete(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = ReferenceListResponseClass.class))), description = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.BOOKING_API_LIST_REFERENCES)
    public ResponseEntity<IRunnerResponse> listBookingReferences(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        List<ReferenceNumbersResponse> referenceNumbersList = referenceNumbersV3Service.list(listCommonRequest, null);
        List<IRunnerResponse> responseList = referenceNumbersList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    @PostMapping(ApiConstants.BOOKING_API_CREATE_PARTIES)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = PartiesConstants.PARTIES_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = PartiesResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> createBookingParties(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        return ResponseHelper.buildSuccessResponse(partiesV3Service.create(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = PartiesConstants.PARTIES_UPDATE_SUCCESSFUL , content = @Content(schema = @Schema(implementation = PartiesResponse.class)))
    })
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_PARTIES)
    public ResponseEntity<IRunnerResponse> updateBookingParties(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        return ResponseHelper.buildSuccessResponse(partiesV3Service.update(partiesRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PartiesConstants.PARTIES_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = PartiesResponse.class)))})
    @PostMapping(ApiConstants.BOOKING_API_DELETE_PARTIES)
    public ResponseEntity<IRunnerResponse> deleteBookingParties(@RequestParam @Valid PartiesRequest partiesRequest) {
        return ResponseHelper.buildSuccessResponse(partiesV3Service.delete(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = PartiesListResponseClass.class))), description = PartiesConstants.PARTIES_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.BOOKING_API_LIST_PARTIES)
    public ResponseEntity<IRunnerResponse> listBookingParties(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        List<PartiesResponse> partiesList = partiesV3Service.list(listCommonRequest);
        List<IRunnerResponse> responseList = partiesList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long bookingId) {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.getAllMasterData(bookingId));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.DEFAULT_BOOKING_GENERATED_SUCCESSFULLY, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @GetMapping(ApiConstants.API_DEFAULT_BOOKING)
    public ResponseEntity<IRunnerResponse> getDefaultBooking() {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.getDefaultBooking());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.RETRIEVE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class)))})
    @PostMapping(ApiConstants.API_CLONE_FROM_SHIPMENT)
        public ResponseEntity<IRunnerResponse> cloneBookingFromShipment(@RequestBody @Valid CloneRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.cloneBookingFromShipmentIfExist(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.RETRIEVE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = CustomerBookingV3Response.class)))})
    @PostMapping(ApiConstants.API_CLONE_BOOKING)
    public ResponseEntity<IRunnerResponse> cloneBookingById(@RequestBody @Valid CloneRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.cloneBookingById(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = CustomerBookingConstants.FETCH_RESET_QUOTE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = QuoteResetRulesResponse.class)))})
    @PostMapping(ApiConstants.API_RESET_QUOTE_FIELDS)
    public ResponseEntity<IRunnerResponse> resetBookingQuoteRules(@RequestParam(value = "bookingId", required = false) Long bookingId) {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.resetBookingQuoteRules(bookingId));
    }
}
