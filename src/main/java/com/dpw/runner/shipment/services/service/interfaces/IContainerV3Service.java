package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Set;

public interface IContainerV3Service {
    ContainerResponse create(ContainerV3Request containerRequest, String module);

    BulkContainerResponse updateBulk(List<ContainerV3Request> request, String module);

    BulkContainerResponse deleteBulk(List<ContainerV3Request> request, String module);

    ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId, String xSource) throws RunnerException;

    List<Containers> findByIdIn(List<Long> containerIds);

    ContainerListResponse fetchShipmentContainers(CommonRequestModel commonRequestModel, String xSource) throws RunnerException;
    ContainerListResponse fetchConsolidationContainers(ListCommonRequest listCommonRequest, String xSource) throws RunnerException;
    ContainerNumberCheckResponse validateContainerNumber(String containerNumber);
    void downloadContainers(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException;
    ContainerListResponse list(ListCommonRequest listCommonRequest, boolean isMasterData, String xSource) throws RunnerException;

    void processContainersAfterShipmentAttachment(
            Long consolidationId,
            List<ShipmentDetails> shipmentDetailsList,
            Set<Long> attachedShipmentIds,
            Set<Long> interBranchRequestedShipIds);

    ContainerResponse assignContainers(AssignContainerRequest request) throws RunnerException;
    ContainerResponse unAssignContainers(UnAssignContainerRequest request) throws RunnerException;

    List<Long> findContainerIdsAttachedToEitherPackingOrShipment(List<Long> containerIds);
}
