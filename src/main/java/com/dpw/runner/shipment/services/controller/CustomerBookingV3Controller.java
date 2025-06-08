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
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
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

    @Autowired
    public CustomerBookingV3Controller(IPackingV3Service packingV3Service,
                                       IContainerV3Service containerV3Service,
                                       IReferenceNumbersV3Service referenceNumbersV3Service,
                                       IPartiesV3Service partiesV3Service,
                                       ICustomerBookingV3Service customerBookingV3Service,
                                       ICRPServiceAdapter crpService) {
        this.containerV3Service = containerV3Service;
        this.packingV3Service = packingV3Service;
        this.referenceNumbersV3Service = referenceNumbersV3Service;
        this.partiesV3Service = partiesV3Service;
        this.customerBookingV3Service = customerBookingV3Service;
        this.crpService = crpService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL, response = CustomerBookingV3Response.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_CREATE + "')")
    public ResponseEntity<IRunnerResponse> createBooking(@RequestBody @Valid CustomerBookingV3Request request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.create(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.UPDATE_SUCCESSFUL, response = CustomerBookingV3Response.class)})
    @PutMapping(ApiConstants.API_UPDATE_BOOKING)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> updateBooking(@RequestBody @Valid CustomerBookingV3Request request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.update(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.DELETE_SUCCESSFUL, response = CustomerBookingV3DeleteResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_MODIFY + "')")
    public ResponseEntity<IRunnerResponse> deleteBooking(@RequestParam @Valid Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.delete(id));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = CustomerBookingV3ListResponse.class, message = CustomerBookingConstants.LIST_SUCCESSFUL, responseContainer = CustomerBookingConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) throws RunnerException {
        CustomerBookingV3ListResponse customerBookingV3ListResponse = customerBookingV3Service.list(listCommonRequest);
        return ResponseHelper.buildListSuccessBookingResponse(customerBookingV3ListResponse.getCustomerBookingV3Responses(), customerBookingV3ListResponse.getTotalPages(), customerBookingV3ListResponse.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = CustomerBookingV3Response.class, message = CustomerBookingConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = CustomerBookingConstants.BOOKING_ID) @RequestParam Optional<Long> id, @ApiParam(value = CustomerBookingConstants.BOOKING_GUID) @RequestParam Optional<String> guid) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.retrieveById(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = CustomerBookingV3Response.class, message = CustomerBookingConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_BOOKING_NUMBER)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_VIEW + "')")
    public ResponseEntity<IRunnerResponse> retrieveById(@RequestParam String bookingNumber) {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.findByBookingNumber(bookingNumber));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.UPDATE_SUCCESSFUL, response = CustomerBookingV3Response.class)})
    @PutMapping(ApiConstants.API_CANCEL_BOOKING)
    @PreAuthorize("hasAuthority('" + PermissionConstants.CUSTOMER_BOOKINGS_CANCEL + "')")
    public ResponseEntity<IRunnerResponse> cancel(@RequestBody @Valid CustomerBookingV3Request request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.update(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL, response = CustomerBookingV3Response.class)})
    @GetMapping(ApiConstants.API_CLONE)
    public ResponseEntity<IRunnerResponse> cloneById(@ApiParam(value = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.cloneBooking(id));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.RETRIEVE_BY_ORDER_ID_SUCCESSFUL, response = CustomerBookingV3Response.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ORDER_ID)
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(@ApiParam(value = CustomerBookingConstants.ORDER_ID, required = true) @RequestParam String orderId) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.retrieveByOrderId(orderId));
    }

    @PostMapping(CustomerBookingConstants.PLATFORM_CREATE_BOOKING)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.CREATE_SUCCESSFUL, response = PlatformToRunnerCustomerBookingResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> platformCreateBooking(@RequestBody @Valid PlatformToRunnerCustomerBookingRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.platformCreateBooking(request));
    }

    @PostMapping(CustomerBookingConstants.CRP_LIST)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> listCRPService(@RequestBody @Valid CRPListRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(crpService.listCRPService(CommonRequestModel.buildRequest(request)));
    }

    @PostMapping(CustomerBookingConstants.CRP_RETRIEVE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CustomerBookingConstants.RETRIEVE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> retrieveCRPService(@RequestBody @Valid CRPRetrieveRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(crpService.retrieveCRPService(CommonRequestModel.buildRequest(request)));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_SUCCESSFUL, response = CheckCreditLimitResponse.class)})
    @PostMapping(CustomerBookingConstants.FUSION_CHECK_CREDIT_LIMIT)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromFusion(@RequestBody @Valid CreditLimitRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.checkCreditLimitFromFusion(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.CREDIT_LIMIT_RETRIEVE_SUCCESSFUL, response = V1ShipmentCreationResponse.class)})
    @GetMapping(CustomerBookingConstants.RETRY_FOR_BILLING)
    public ResponseEntity<IRunnerResponse> retryForBilling(@ApiParam(value = CustomerBookingConstants.BOOKING_ID, required = true) @RequestParam Long id) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.retryForBilling(CommonGetRequest.builder().id(id).build()));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, response = ContainerResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_CREATE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> createBookingContainers(@RequestBody @Valid ContainerV3Request containerV3Request) throws RunnerException {
        ContainerResponse containerResponse = containerV3Service.create(containerV3Request, BOOKING);
        return ResponseHelper.buildSuccessResponse(containerResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL, response = BulkContainerResponse.class)})
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> updateBookingContainers(@RequestBody @Valid List<ContainerV3Request> containerV3Requests) throws RunnerException {
        BulkContainerResponse containerResponse = containerV3Service.updateBulk(containerV3Requests, BOOKING);
        return ResponseHelper.buildSuccessResponse(containerResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, response = ContainerListResponse.class)})
    @GetMapping(ApiConstants.BOOKING_API_LIST_CONTAINERS)
    public ResponseEntity<IRunnerResponse> listBookingContainers(@RequestBody ListCommonRequest listCommonRequest) throws RunnerException {
        ContainerListResponse containerListResponse = containerV3Service.list(listCommonRequest, true, null);
        return ResponseHelper.buildSuccessResponse(containerListResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_DELETE_SUCCESSFUL, response = BulkContainerResponse.class)})
    @DeleteMapping(ApiConstants.BOOKING_API_DELETE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> deleteBookingContainers(@RequestBody @Valid List<ContainerV3Request> containerV3Requests) {
        BulkContainerResponse bulkContainerResponse = containerV3Service.deleteBulk(containerV3Requests, BOOKING);
        return ResponseHelper.buildSuccessResponse(bulkContainerResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL, response = PackingResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_CREATE_PACKAGES)
    public ResponseEntity<IRunnerResponse> createBookingPackages(@RequestBody @Valid PackingV3Request packingV3Request) throws RunnerException {
        PackingResponse packingResponse = packingV3Service.create(packingV3Request, BOOKING);
        return ResponseHelper.buildSuccessResponse(packingResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_PACKAGES)
    public ResponseEntity<IRunnerResponse> updateBookingPackages(@RequestBody @Valid List<PackingV3Request> packingV3RequestList) throws RunnerException {
        BulkPackingResponse bulkPackingResponse = packingV3Service.updateBulk(packingV3RequestList, BOOKING);
        return ResponseHelper.buildSuccessResponse(bulkPackingResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_LIST_SUCCESSFUL, response = PackingListResponse.class)})
    @GetMapping(ApiConstants.BOOKING_API_LIST_PACKAGES)
    public ResponseEntity<IRunnerResponse> listBookingPackages(@RequestBody ListCommonRequest listCommonRequest) {
        PackingListResponse packingListResponse = packingV3Service.list(listCommonRequest, true, null);
        return ResponseHelper.buildSuccessResponse(packingListResponse);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @DeleteMapping(ApiConstants.BOOKING_API_DELETE_PACKAGES)
    public ResponseEntity<IRunnerResponse> deleteBookingPackages(@RequestBody @Valid List<PackingV3Request> packingV3RequestList) throws RunnerException {
        BulkPackingResponse response = packingV3Service.deleteBulk(packingV3RequestList, BOOKING);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @PostMapping(ApiConstants.BOOKING_API_CREATE_REFERENCES)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_CREATE_SUCCESSFUL, response = ReferenceNumbersResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> createBookingReferences(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.create(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_UPDATE_SUCCESSFUL, response = ReferenceNumbersResponse.class)})
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_REFERENCES)
    public ResponseEntity<IRunnerResponse> updateBookingReferences(@RequestBody @Valid @NonNull ReferenceNumbersRequest request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.update(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_DELETE_SUCCESSFUL, response = ReferenceNumbersResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_DELETE_REFERENCES)
    public ResponseEntity<IRunnerResponse> deleteBookingReferences(@RequestBody @Valid ReferenceNumbersRequest request) {
        return ResponseHelper.buildSuccessResponse(referenceNumbersV3Service.delete(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = ReferenceListResponseClass.class, message = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL, responseContainer = ReferenceNumbersConstants.REFERENCE_NUMBERS_LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.BOOKING_API_LIST_REFERENCES)
    public ResponseEntity<IRunnerResponse> listBookingReferences(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        List<ReferenceNumbersResponse> referenceNumbersList = referenceNumbersV3Service.list(listCommonRequest, null);
        List<IRunnerResponse> responseList = referenceNumbersList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    @PostMapping(ApiConstants.BOOKING_API_CREATE_PARTIES)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PartiesConstants.PARTIES_CREATE_SUCCESSFUL, response = PartiesResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> createBookingParties(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        return ResponseHelper.buildSuccessResponse(partiesV3Service.create(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PartiesConstants.PARTIES_UPDATE_SUCCESSFUL , response = PartiesResponse.class)
    })
    @PutMapping(ApiConstants.BOOKING_API_UPDATE_PARTIES)
    public ResponseEntity<IRunnerResponse> updateBookingParties(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        return ResponseHelper.buildSuccessResponse(partiesV3Service.update(partiesRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PartiesConstants.PARTIES_DELETE_SUCCESSFUL, response = PartiesResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_DELETE_PARTIES)
    public ResponseEntity<IRunnerResponse> deleteBookingParties(@RequestParam @Valid PartiesRequest partiesRequest) {
        return ResponseHelper.buildSuccessResponse(partiesV3Service.delete(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = PartiesListResponseClass.class, message = PartiesConstants.PARTIES_LIST_SUCCESSFUL, responseContainer = PartiesConstants.PARTIES_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.BOOKING_API_LIST_PARTIES)
    public ResponseEntity<IRunnerResponse> listBookingParties(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        List<PartiesResponse> partiesList = partiesV3Service.list(listCommonRequest);
        List<IRunnerResponse> responseList = partiesList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CustomerBookingConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long bookingId) {
        return ResponseHelper.buildSuccessResponse(customerBookingV3Service.getAllMasterData(bookingId));
    }
}
