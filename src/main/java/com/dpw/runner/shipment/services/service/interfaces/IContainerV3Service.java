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
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import java.util.Set;
import javax.servlet.http.HttpServletResponse;

public interface IContainerV3Service {
    ContainerResponse create(ContainerV3Request containerRequest, String module);

    BulkContainerResponse updateBulk(List<ContainerV3Request> request, String module);

    BulkContainerResponse deleteBulk(List<ContainerV3Request> request, String module);

    ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId) throws RunnerException;
    ContainerListResponse fetchShipmentContainers(CommonRequestModel commonRequestModel) throws RunnerException;
    ContainerNumberCheckResponse validateContainerNumber(String containerNumber);
    void downloadContainers(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException;
    ContainerListResponse list(ListCommonRequest listCommonRequest, boolean isMasterData) throws RunnerException;

    void processContainersAfterShipmentAttachment(
            Long consolidationId,
            List<ShipmentDetails> shipmentDetailsList,
            Set<Long> attachedShipmentIds,
            Set<Long> interBranchRequestedShipIds);

    ContainerResponse assignContainers(AssignContainerRequest request) throws RunnerException;

}
