package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ELDetailsConstants;
import com.dpw.runner.shipment.services.commons.constants.FileRepoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EntityIdAndTypeRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.request.UploadDocumentRequest;
import com.dpw.runner.shipment.services.dto.response.ELDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.FileRepoResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IFileRepoService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping(FileRepoConstants.FILE_REPO_API_HANDLE)
@Slf4j
public class FileRepoController {
    @Autowired
    private IFileRepoService fileRepoService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = FileRepoConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<FileRepoResponse>> create(@RequestBody @Valid FileRepoRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<FileRepoResponse>>) fileRepoService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<FileRepoResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_UPDATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid FileRepoRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) fileRepoService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_DELETE_SUCCESSFUL) })
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) fileRepoService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_RETRIEVE_BY_ID_SUCCESSFUL) })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<FileRepoResponse>> retrieveById(@ApiParam(value = FileRepoConstants.FILE_REPO_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<FileRepoResponse>>) fileRepoService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID_PARTIAL)
    public ResponseEntity<?> retrieveByIdPartial(@RequestParam(name = "includeColumns", required = false) List<String> includeColumns, @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        try {
            ResponseEntity<RunnerResponse<FileRepoResponse>> fileRepo = (ResponseEntity<RunnerResponse<FileRepoResponse>>) fileRepoService.retrieveById(CommonRequestModel.buildRequest(request));
            return PartialFetchUtils.fetchPartialData(fileRepo,includeColumns);
        } catch (Exception ex) {
            System.out.println(ex.toString());
        }
        return ResponseEntity.ok(null);
    }


    @PostMapping(FileRepoConstants.FILE_REPO_LIST_ENTITYID_ENTITYTYPE)
    public ResponseEntity<RunnerListResponse<FileRepoResponse>> retrieveByIdAndType(@RequestBody @Valid EntityIdAndTypeRequest entityIdAndTypeRequest){
        return (ResponseEntity<RunnerListResponse<FileRepoResponse>>) fileRepoService.retrieveByEntityIdAndEntityType(CommonRequestModel.buildRequest(entityIdAndTypeRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_LIST_SUCCESSFUL, responseContainer = FileRepoConstants.RESPONSE_CONTAINER_LIST) })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<FileRepoResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<FileRepoResponse>>) fileRepoService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = FileRepoConstants.UPLOAD_DOCUMENT_SUCCESSFUL) })
    @PostMapping(FileRepoConstants.UPLOAD_DOCUMENT)
    public ResponseEntity<RunnerListResponse<FileRepoResponse>> uploadDocument(@RequestParam List<MultipartFile> files, @RequestParam Long entityId, @RequestParam String entityType, @RequestParam String docType, @RequestParam Boolean clientEnabled, @RequestParam String eventCode) {
        UploadDocumentRequest uploadDocumentRequest = UploadDocumentRequest.builder().files(files).entityId(entityId).entityType(entityType).docType(docType).clientEnabled(clientEnabled).eventCode(eventCode).build();
        return (ResponseEntity<RunnerListResponse<FileRepoResponse>>) fileRepoService.uploadDocument(CommonRequestModel.buildRequest(uploadDocumentRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = FileRepoConstants.DOWNLOAD_DOCUMENT_SUCCESSFUL) })
    @GetMapping(FileRepoConstants.DOWNLOAD_DOCUMENT)
    public ResponseEntity<?> downloadDocument(@RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return fileRepoService.downloadDocument(CommonRequestModel.buildRequest(request));
    }
}
