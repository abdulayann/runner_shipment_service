package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface IPackingService extends ICommonService {
    void uploadPacking(BulkUploadRequest request) throws Exception;

    void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws Exception;

    ResponseEntity<IRunnerResponse> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception;

    ResponseEntity<IRunnerResponse> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listPacksToDetach(CommonRequestModel commonRequestModel) throws Exception;

    PackSummaryResponse calculatePackSummary(List<Packing> packingList, String transportMode, String containerCategory, ShipmentMeasurementDetailsDto dto) throws Exception;

    ResponseEntity<IRunnerResponse> autoCalculateVolumetricWeight(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> autoCalculateChargable(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> autoCalculateVolume(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> calculateVolumetricWeightForAirAndChargeable(CommonRequestModel commonRequestModel) throws Exception;
}
