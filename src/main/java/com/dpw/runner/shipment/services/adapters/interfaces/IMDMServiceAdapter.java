package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskApproveOrRejectRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.response.mdm.MDMTaskRetrieveResponse;
import com.dpw.runner.shipment.services.dto.response.mdm.MdmTaskCreateResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Map;

public interface IMDMServiceAdapter {
    ResponseEntity<IRunnerResponse> getCreditInfo(CommonRequestModel commonRequestModel) throws RunnerException;

    String getApprovalStausForParties(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> createShipmentTaskFromBooking(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> createNonBillableCustomer(CommonRequestModel commonRequestModel) throws RunnerException;

    List<Map<String, Object>> getDepartmentList(String transportMode, String shipmentType, String module);

    DependentServiceResponse getContainerTypes() throws RunnerException;

    MdmTaskCreateResponse createTask(MdmTaskCreateRequest request) throws RunnerException;

    void approveOrRejectTask(MdmTaskApproveOrRejectRequest request) throws RunnerException;

    List<Map<String, Object>> getTaskList(String entityUuid, String entityType, String status, String taskType);

    MDMTaskRetrieveResponse getTask(String taskUuid, Long id) throws RunnerException;

}
