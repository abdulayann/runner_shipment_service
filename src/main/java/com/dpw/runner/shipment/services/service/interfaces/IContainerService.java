package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.commons.requests.ExportContainerListRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

public interface IContainerService extends ICommonService {
    ResponseEntity<IRunnerResponse> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> calculateAllocatedData(CommonRequestModel commonRequestModel);

//    ResponseEntity<?> calculateAchievedQuantity_onPackAssign(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getContainersForSelection(CommonRequestModel commonRequestModel);

    void uploadContainers(BulkUploadRequest request) throws RunnerException, IOException;

    void uploadContainerEvents(BulkUploadRequest request) throws RunnerException, IOException;

    void downloadContainers(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException;

    void downloadContainerEvents(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException, IOException, IllegalAccessException;


    ResponseEntity<IRunnerResponse> V1ContainerCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;

    void afterSave(Containers containers, boolean isCreate);

    void afterSaveList(List<Containers> containers, boolean isCreate);

    void exportContainers(HttpServletResponse response, ExportContainerListRequest request) throws RunnerException, IOException, IllegalAccessException;

    ResponseEntity<IRunnerResponse> V1BulkContainerCreateAndUpdate(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> validateContainerNumber(String containerNumber);

    ResponseEntity<IRunnerResponse> getContainers(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> checkForDelete(CommonRequestModel commonRequestModel);

    ContainerSummaryResponse calculateContainerSummary(List<Containers> containersList, String transportMode, String containerCategory) throws RunnerException;
    Containers calculateUtilization(Containers container);
    ResponseEntity<IRunnerResponse> containerSync(List<Long> request);
    void pushContainersToDependentServices(List<Containers> containersList, List<Containers> oldContainers);
}
