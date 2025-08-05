package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;

import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Set;

public interface IContainerV3Service {
    ContainerResponse create(ContainerV3Request containerRequest, String module) throws RunnerException;
    BulkContainerResponse createBulk(List<ContainerV3Request> containerV3Requests, String module) throws RunnerException;

    BulkContainerResponse updateBulk(List<ContainerV3Request> request, String module) throws RunnerException;

    BulkContainerResponse deleteBulk(List<ContainerV3Request> request, String module) throws RunnerException;

    ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId, String xSource) throws RunnerException;

    List<Containers> findByIdIn(List<Long> containerIds);

    ContainerListResponse fetchShipmentContainers(ListCommonRequest commonRequestModel, String xSource) throws RunnerException;
    ContainerListResponse fetchConsolidationContainers(ListCommonRequest listCommonRequest, String xSource) throws RunnerException;
    ContainerNumberCheckResponse validateContainerNumber(String containerNumber);
    void downloadContainers(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException;
    ContainerListResponse list(ListCommonRequest listCommonRequest, boolean isMasterData, String xSource) throws RunnerException;

    void processContainersAfterShipmentAttachment(
            Long consolidationId,
            List<ShipmentDetails> shipmentDetailsList,
            Set<Long> attachedShipmentIds,
            Set<Long> interBranchRequestedShipIds);

    ContainerResponse assignContainers(AssignContainerRequest request, String module) throws RunnerException;
    ContainerResponse unAssignContainers(UnAssignContainerRequest request, String module) throws RunnerException;

    List<Long> findContainerIdsAttachedToEitherPackingOrShipment(List<Long> containerIds);
    void updateAttachedContainersData(List<Long> containerIds) throws RunnerException;
    void addShipmentCargoToContainer(Containers container, ShipmentDetails shipmentDetails) throws RunnerException;
    void addShipmentCargoToContainerInCreateFromBooking(Containers container, CustomerBookingV3Request customerBookingV3Request) throws RunnerException;

    List<ContainerInfoProjection> getContainers(List<Long> containerIds);
    void pushContainersToDependentServices(List<Containers> containersList);
    ContainerListResponse fetchConsolidationContainersForPackageAssignment(ListCommonRequest request) throws RunnerException;
    void addPackageDataToContainer(Containers container, Packing packing) throws RunnerException;
    ContainerSummaryResponse getContainerSummaryResponse(List<Containers> containersList, boolean isShipment, String xSource) throws RunnerException;
}
