package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.FileRepoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IFileRepoDao;
import com.dpw.runner.shipment.services.dto.request.EntityIdAndTypeRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.request.UploadDocumentRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.FileRepoResponse;
import com.dpw.runner.shipment.services.dto.response.UploadDocumentResponse;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IFileRepoService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class FileRepoService implements IFileRepoService {
    @Autowired
    private IFileRepoDao fileRepoDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private DocumentService documentService;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        FileRepoRequest request = null;
        request = (FileRepoRequest) commonRequestModel.getData();
        FileRepo fileRepo = mapToEntityFromRequest(request);
        fileRepo = fileRepoDao.save(fileRepo);
        if(request == null) {
            log.debug("Request is empty for File create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        try {
            fileRepo = fileRepoDao.save(fileRepo);
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(fileRepo)
                            .prevData(null)
                            .parent(FileRepo.class.getSimpleName())
                            .parentId(fileRepo.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("File created for successfully for Id {} with Request Id {}", fileRepo.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertToResponse(fileRepo));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        FileRepoRequest request = (FileRepoRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for File update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for File update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<FileRepo> oldEntity = fileRepoDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("File Repo is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        FileRepo fileRepo = mapToEntityFromRequest(request);
        fileRepo.setId(oldEntity.get().getId());
        if(fileRepo.getGuid() != null && !oldEntity.get().getGuid().equals(fileRepo.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            fileRepo = fileRepoDao.save(fileRepo);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(fileRepo)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, FileRepo.class))
                            .parent(FileRepo.class.getSimpleName())
                            .parentId(fileRepo.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the File details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertToResponse(fileRepo));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for list File with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<FileRepo>, Pageable> tuple = fetchData(request, FileRepo.class);
            Page<FileRepo> FileRepoPage  = fileRepoDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("File list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertListResponse(FileRepoPage.getContent()),
                    FileRepoPage.getTotalPages(),
                    FileRepoPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for async list File with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            log.info("Retrieving File details");
            Pair<Specification<FileRepo>, Pageable> tuple = fetchData(request, FileRepo.class);
            Page<FileRepo> FileRepoPage  = fileRepoDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("File async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertListResponse(FileRepoPage.getContent()),
                    FileRepoPage.getTotalPages(),
                    FileRepoPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<FileRepo> fileRepo = fileRepoDao.findById(id);
            if(!fileRepo.isPresent()) {
                log.debug("File Repo is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(fileRepo.get());
            fileRepoDao.delete(fileRepo.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, FileRepo.class))
                            .parent(FileRepo.class.getSimpleName())
                            .parentId(fileRepo.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted file for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for File retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for File retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<FileRepo> fileRepo = fileRepoDao.findById(id);
            if(!fileRepo.isPresent()) {
                log.debug("File Repo is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("File details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            FileRepoResponse response = convertToResponse(fileRepo.get());
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
            return ResponseHelper.buildSuccessResponse(response);
            else
            return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveByEntityIdAndEntityType(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            EntityIdAndTypeRequest request = (EntityIdAndTypeRequest) commonRequestModel.getData();
            List<FileRepo> fileRepoList = fileRepoDao.findByEntityIdAndEntityType(request.getEntityId(), request.getEntityType());
            if (fileRepoList.size() == 0) {
                ResponseHelper.buildListSuccessResponse(null);
            }
            return ResponseHelper.buildListSuccessResponse(convertListResponse(fileRepoList));

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertListResponse(List<FileRepo> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(fileRepo -> {
            responseList.add(convertToResponse(fileRepo));
        });
        return responseList;
    }

    private FileRepo mapToEntityFromRequest(FileRepoRequest request) {
        return jsonHelper.convertValue(request, FileRepo.class);
    }

    private FileRepoResponse convertToResponse(FileRepo fileRepo) {
        return jsonHelper.convertValue(fileRepo, FileRepoResponse.class);
    }

    @Override
    public ResponseEntity<?> uploadDocument(CommonRequestModel commonRequestModel) {
        UploadDocumentRequest uploadDocumentRequest = (UploadDocumentRequest) commonRequestModel.getData();

        List<MultipartFile> files = uploadDocumentRequest.getFiles();
        Long entityId = uploadDocumentRequest.getEntityId();
        String entityType = uploadDocumentRequest.getEntityType();
        List<IRunnerResponse> responseBodyList = new ArrayList<>();
        if(files != null) {
            for (var file : files) {
                String filename = file.getOriginalFilename();
                Integer tenantId = UserContext.getUser().TenantId;
                String path = tenantId.toString() + "/" + entityType + "/" + entityId.toString() + "/" + UUID.randomUUID().toString();
                ResponseEntity<UploadDocumentResponse> responseBody;
                try {
                    responseBody = documentService.PostDocument(file, path);
                    if (responseBody.getStatusCode() != HttpStatus.OK && responseBody.getStatusCode() != HttpStatus.CREATED) {
                        String responseMsg = FileRepoConstants.UPLOAD_DOCUMENT_FAILED + " : " + responseBody.getBody();
                        return ResponseHelper.buildFailedResponse(responseMsg);
                    }
                } catch (Exception e) {
                    String responseMsg = e.getMessage() != null ? e.getMessage()
                            : FileRepoConstants.UPLOAD_DOCUMENT_FAILED;
                    log.error(responseMsg, e);
                    return ResponseHelper.buildFailedResponse(responseMsg);
                }
                FileRepoRequest fileRepoRequest = FileRepoRequest.builder().fileName(filename).path(responseBody.getBody().getPath()).
                        entityId(entityId).entityType(entityType).docType(uploadDocumentRequest.getDocType()).
                        clientEnabled(uploadDocumentRequest.getClientEnabled()).eventCode(uploadDocumentRequest.getEventCode()).build();
                ResponseEntity<RunnerResponse<EventsResponse>> response = (ResponseEntity<RunnerResponse<EventsResponse>>) create(CommonRequestModel.buildRequest(fileRepoRequest));
                responseBodyList.add(response.getBody().getData());
            }
        } else if(uploadDocumentRequest.getFileResource() != null) {
            ByteArrayResource file = uploadDocumentRequest.getFileResource();
            String filename = file.getFilename();
            Integer tenantId = UserContext.getUser().TenantId;
            String path = tenantId.toString() + "/" + entityType + "/" + entityId.toString() + "/" + UUID.randomUUID().toString();
            ResponseEntity<UploadDocumentResponse> responseBody;
            try {
                responseBody = documentService.PostDocument(file, path);
                if (responseBody.getStatusCode() != HttpStatus.OK && responseBody.getStatusCode() != HttpStatus.CREATED) {
                    String responseMsg = FileRepoConstants.UPLOAD_DOCUMENT_FAILED + " : " + responseBody.getBody();
                    return ResponseHelper.buildFailedResponse(responseMsg);
                }
            } catch (Exception e) {
                String responseMsg = e.getMessage() != null ? e.getMessage()
                        : FileRepoConstants.UPLOAD_DOCUMENT_FAILED;
                log.error(responseMsg, e);
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
            FileRepoRequest fileRepoRequest = FileRepoRequest.builder().fileName(filename).path(responseBody.getBody().getPath()).
                    entityId(entityId).entityType(entityType).docType(uploadDocumentRequest.getDocType()).
                    clientEnabled(uploadDocumentRequest.getClientEnabled()).eventCode(uploadDocumentRequest.getEventCode()).build();
            ResponseEntity<RunnerResponse<EventsResponse>> response = (ResponseEntity<RunnerResponse<EventsResponse>>) create(CommonRequestModel.buildRequest(fileRepoRequest));
            responseBodyList.add(response.getBody().getData());
        }
        return ResponseHelper.buildListSuccessResponse(responseBodyList);
    }

    public ResponseEntity<?> downloadDocument(CommonRequestModel commonRequestModel) {
        FileRepoResponse response = (FileRepoResponse) ((RunnerResponse) retrieveById(commonRequestModel).getBody()).getData();
        String path = response.getPath();
        ResponseEntity<?> responseBody;
        try {
            responseBody = documentService.DownloadDocument(path);
            if(responseBody.getStatusCode() != HttpStatus.OK) {
                String responseMsg = FileRepoConstants.DOWNLOAD_DOCUMENT_FAILED + " : " + responseBody.getBody();
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : FileRepoConstants.DOWNLOAD_DOCUMENT_FAILED;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return responseBody;
    }
}
