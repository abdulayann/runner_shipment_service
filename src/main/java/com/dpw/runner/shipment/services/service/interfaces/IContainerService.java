package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.commons.requests.ExportContainerListRequest;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface IContainerService extends ICommonService {
    ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);

    ResponseEntity<?> calculateAchievedQuantity_onPackAssign(CommonRequestModel commonRequestModel);

    ResponseEntity<?> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel);

    ResponseEntity<?> getContainersForSelection(CommonRequestModel commonRequestModel);

    void uploadContainers(BulkUploadRequest request) throws Exception;

    void uploadContainerEvents(BulkUploadRequest request) throws Exception;

    void downloadContainers(HttpServletResponse response, BulkDownloadRequest request) throws Exception;

    void downloadContainerEvents(HttpServletResponse response, BulkDownloadRequest request) throws Exception;

    ResponseEntity<?> V1ContainerCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception;

    void afterSave(Containers containers, boolean isCreate);

    void afterSaveList(List<Containers> containers, boolean isCreate);

    void exportContainers(HttpServletResponse response, ExportContainerListRequest request) throws Exception;
}
