package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;

public interface IPackingService extends ICommonService {
    void uploadPacking(BulkUploadRequest request) throws Exception;

    void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws Exception;

    ResponseEntity<?> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception;

    ResponseEntity<?> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel);
    ResponseEntity<?> listPacksToDetach(CommonRequestModel commonRequestModel) throws Exception;

}
