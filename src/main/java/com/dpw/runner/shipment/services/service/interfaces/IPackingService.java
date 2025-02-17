package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface IPackingService extends ICommonService {
    void uploadPacking(BulkUploadRequest request) throws RunnerException;

    void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException;

    ResponseEntity<IRunnerResponse> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;

    ResponseEntity<IRunnerResponse> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listPacksToDetach(CommonRequestModel commonRequestModel) throws RunnerException;

    PackSummaryResponse calculatePackSummary(List<Packing> packingList, String transportMode, String containerCategory, ShipmentMeasurementDetailsDto dto) throws RunnerException;

    ResponseEntity<IRunnerResponse> autoCalculatePacksData(CommonRequestModel commonRequestModel);

    PackSummaryResponse calculatePacksUtilisationForConsolidation(CalculatePackUtilizationRequest calculatePackUtilizationRequest) throws RunnerException;

    void savePackUtilisationCalculationInConsole(CalculatePackUtilizationRequest calculatePackUtilizationRequest);
}
