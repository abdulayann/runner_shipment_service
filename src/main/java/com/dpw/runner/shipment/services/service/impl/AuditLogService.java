package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AuditLogConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.commons.dto.response.AuditLogResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.ExcelUtils;
import com.dpw.runner.shipment.services.utils.ExcludeAuditLog;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.jetbrains.annotations.NotNull;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.persistence.*;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class AuditLogService implements IAuditLogService {
    private static final Set<Class<?>> annotationClassList = new HashSet<>(Arrays.asList(Id.class, OneToMany.class, ManyToOne.class, ManyToMany.class, ExcludeAuditLog.class));

    public static Map<String, String> COLUMN_HEADERS_TO_FIELD_NAME = null;

    static {
        COLUMN_HEADERS_TO_FIELD_NAME = new LinkedHashMap<>();
        COLUMN_HEADERS_TO_FIELD_NAME.put("Module", "moduleTypeCode");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Module id", "moduleId");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Field Name", "fieldName");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Old value", AuditLogConstants.OLD_VALUE);
        COLUMN_HEADERS_TO_FIELD_NAME.put("New value", AuditLogConstants.NEW_VALUE);
        COLUMN_HEADERS_TO_FIELD_NAME.put("User Name", Constants.CREATED_BY);
        COLUMN_HEADERS_TO_FIELD_NAME.put("Changed Date", Constants.CREATED_AT);
    }

    public ExecutorService executorService = Executors.newFixedThreadPool(10);
    @Autowired
    private IAuditLogDao auditLogDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ExcelUtils excelUtils;
    @Autowired
    private CommonUtils commonUtils;

    public Resource downloadExcel(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            var triplet = fetchList(commonRequestModel);
            List<Map<String, Object>> listAsMap = getData(triplet.getLeft());
            Resource fileResource = excelUtils.createExcelAsResource(listAsMap, COLUMN_HEADERS_TO_FIELD_NAME, "Audit_Logs");
            return fileResource;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public List<Map<String, Object>> getData(List<IRunnerResponse> auditLogResponses) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (IRunnerResponse item : auditLogResponses) {
            AuditLogResponse ct = (AuditLogResponse) item;
            Map<String, Object> asMap = new HashMap<>();
            asMap.put("moduleTypeCode", ct.getEntity());
            asMap.put("moduleId", ct.getEntityId().toString());
            asMap.put(Constants.CREATED_BY, ct.getCreatedBy());
            asMap.put(Constants.CREATED_AT, DateUtils.getDateAsString(ct.getCreatedAt()));
            //field name, old value, new value
            JsonNode nodeChanges = jsonHelper.convertValue(ct.getChanges(), JsonNode.class);
            Iterator<String> fieldNames = nodeChanges.fieldNames();
            while (fieldNames.hasNext()) {
                Map<String, Object> clone = new HashMap<>(asMap);
                String field = fieldNames.next();
                clone.put("fieldName", field);
                clone.put(AuditLogConstants.OLD_VALUE, nodeChanges.get(field).get(AuditLogConstants.OLD_VALUE).asText());
                clone.put(AuditLogConstants.NEW_VALUE, nodeChanges.get(field).get(AuditLogConstants.NEW_VALUE).asText());
                result.add(clone);
            }
        }
        return result;
    }

    public void addAuditLog(AuditLogMetaData auditLogMetaData) throws IllegalAccessException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, NoSuchMethodException, RunnerException {
        String skipAuditLog = MDC.get("skip-audit-log");
        if(skipAuditLog != null && skipAuditLog.equals("true"))
            return;
        validateRequest(auditLogMetaData);
        AuditLog auditLog = new AuditLog();
        auditLog.setOperation(auditLogMetaData.getOperation());
        auditLog.setParentType(auditLogMetaData.getParent());
        auditLog.setParentId(auditLogMetaData.getParentId());

        String ops = auditLogMetaData.getOperation();

        if (ops.equals(DBOperationType.CREATE.name())) {
            Map<String, AuditLogChanges> auditLogChangesMap = getChanges(auditLogMetaData.getNewData(), null, ops);
            if(auditLogChangesMap.size() > 0)
                addBaseEntityFields(auditLogChangesMap, null, auditLogMetaData.getNewData());
            auditLog.setChanges(auditLogChangesMap);
            auditLog.setEntity(auditLogMetaData.getNewData().getClass().getSimpleName());
            auditLog.setEntityId(getEntityId(auditLogMetaData.getNewData()));
        } else if (ops.equals(DBOperationType.UPDATE.name())) {
            Map<String, AuditLogChanges> auditLogChangesMap = getChanges(auditLogMetaData.getNewData(), auditLogMetaData.getPrevData(), ops);
            if(auditLogChangesMap.size() > 0)
                addBaseEntityFields(auditLogChangesMap, auditLogMetaData.getPrevData(), auditLogMetaData.getNewData());
            auditLog.setChanges(auditLogChangesMap);
            auditLog.setEntity(auditLogMetaData.getNewData().getClass().getSimpleName());
            auditLog.setEntityId(getEntityId(auditLogMetaData.getNewData()));
        } else if (ops.equals(DBOperationType.DELETE.name())) {
            Map<String, AuditLogChanges> auditLogChangesMap = getChanges(null, auditLogMetaData.getPrevData(), ops);
            if(auditLogChangesMap.size() > 0)
                addBaseEntityFields(auditLogChangesMap, auditLogMetaData.getPrevData(), null);
            auditLog.setChanges(auditLogChangesMap);
            auditLog.setEntity(auditLogMetaData.getPrevData().getClass().getSimpleName());
            auditLog.setEntityId(getEntityId(auditLogMetaData.getPrevData()));
        } else {
            throw new RunnerException("Not a valid operation performed");
        }
        if(ops.equals(DBOperationType.UPDATE.name()) && (auditLog.getChanges() == null || auditLog.getChanges().size() == 0))
            return;
        auditLogDao.save(auditLog);
    }

    public void addBaseEntityFields(Map<String, AuditLogChanges> auditLogChangesMap, BaseEntity oldEntity, BaseEntity newEntity)
    {
        if(newEntity == null)
            return;
        if(oldEntity == null)
        {
            auditLogChangesMap.put(Constants.CREATED_AT, createAuditLogChangesObject(Constants.CREATED_AT, LocalDateTime.now().toString(), null));
            auditLogChangesMap.put(Constants.CREATED_BY, createAuditLogChangesObject(Constants.CREATED_BY, UserContext.getUser().Username, null));
        }
        else
        {
            auditLogChangesMap.put("updatedAt", createAuditLogChangesObject("updatedAt", LocalDateTime.now().toString(), oldEntity.getUpdatedAt() != null ? oldEntity.getUpdatedAt().toString() : null));
            if(!Objects.equals(UserContext.getUser().Username, oldEntity.getUpdatedBy()))
                auditLogChangesMap.put("updatedBy", createAuditLogChangesObject("updatedBy", UserContext.getUser().Username, oldEntity.getUpdatedBy()));
        }
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            var triplet = fetchList(commonRequestModel);
            return ResponseHelper.buildListSuccessResponse(
                    triplet.getLeft(),
                    triplet.getMiddle(),
                    triplet.getRight());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Triple<List<IRunnerResponse>, Integer, Long> fetchList(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for audit log list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Pair<Specification<AuditLog>, Pageable> tuple = fetchData(request, AuditLog.class);
        Page<AuditLog> auditLogPage = auditLogDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Audit log list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        return Triple.of(
                convertEntityListToDtoList(auditLogPage.getContent()),
                auditLogPage.getTotalPages(),
                auditLogPage.getTotalElements()
        );
    }

    public Long getEntityId(BaseEntity entity) throws NoSuchFieldException, IllegalAccessException {
        Field f = entity.getClass().getSuperclass().getSuperclass().getDeclaredField("id");
        f.setAccessible(true);

        return (Long) f.get(entity);
    }

    public List<IRunnerResponse> convertEntityListToDtoList(final List<AuditLog> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }

    public AuditLogResponse convertEntityToDto(AuditLog auditLog) {
        // return jsonHelper.convertValue(auditLog, AuditLogResponse.class);
        AuditLogResponse response = new AuditLogResponse();
        response.setId(auditLog.getId());
        response.setOperation(auditLog.getOperation());
        response.setEntityId(auditLog.getEntityId());
        response.setEntity(auditLog.getEntity());
        response.setParentId(auditLog.getParentId());
        response.setParentType(auditLog.getParentType());
        response.setCreatedAt(auditLog.getCreatedAt());
        response.setCreatedBy(auditLog.getCreatedBy());

        List<AuditLogChanges> changes = new ArrayList<>();
        for (Map.Entry<String, AuditLogChanges> entry : auditLog.getChanges().entrySet()) {
            AuditLogChanges auditChange = entry.getValue();
            auditChange.setFieldName(replaceFieldNames(auditLog, entry.getKey()));
            changes.add(entry.getValue());
        }

        response.setChanges(changes);
        return response;
    }

    public String replaceFieldNames(AuditLog auditLog, String key){
        if(Objects.equals(auditLog.getParentType(), ShipmentDetails.class.getSimpleName())) {
            if (Objects.equals(auditLog.getEntity(), ShipmentDetails.class.getSimpleName())) {
                if (AuditLogConstants.ShipmentsFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.ShipmentsFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), Containers.class.getSimpleName())) {
                if (AuditLogConstants.ContainerFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.ContainerFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), Packing.class.getSimpleName())) {
                if (AuditLogConstants.PackingFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.PackingFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), Routings.class.getSimpleName())) {
                if (AuditLogConstants.RoutingsFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.RoutingsFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), Notes.class.getSimpleName())) {
                if (AuditLogConstants.NotesFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.NotesFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), BookingCarriage.class.getSimpleName())) {
                if (AuditLogConstants.BookingCarriageFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.BookingCarriageFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), Events.class.getSimpleName())) {
                if (AuditLogConstants.EventsFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.EventsFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), ReferenceNumbers.class.getSimpleName())) {
                if (AuditLogConstants.ReferenceNumbersFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.ReferenceNumbersFieldNameToDisplayNameMap.get(key);
                }
            } else if (Objects.equals(auditLog.getEntity(), Parties.class.getSimpleName())) {
                if (AuditLogConstants.PartiesFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.PartiesFieldNameToDisplayNameMap.get(key);
                }
            }
            else if(Objects.equals(auditLog.getEntity(), ServiceDetails.class.getSimpleName())) {
                if (AuditLogConstants.ServiceDetailsFieldNameToDisplayNameMap.containsKey(key)) {
                    return AuditLogConstants.ServiceDetailsFieldNameToDisplayNameMap.get(key);
                }
            }
            else if(Objects.equals(auditLog.getEntity(), TruckDriverDetails.class.getSimpleName())) {
                if (AuditLogConstants.TruckDriverDetailsFieldNameToDisplayName.containsKey(key)) {
                    return AuditLogConstants.TruckDriverDetailsFieldNameToDisplayName.get(key);
                }
            }
        }
        return key;
    }

    private void validateRequest(@NotNull AuditLogMetaData auditLogMetaData) throws RunnerException {
        if (ObjectUtils.isEmpty(auditLogMetaData.getParent()) || ObjectUtils.isEmpty(auditLogMetaData.getParentId())) {
            throw new RunnerException("Parent or parent id is missing");
        } else if (ObjectUtils.isEmpty(auditLogMetaData.getNewData()) && ObjectUtils.isEmpty(auditLogMetaData.getPrevData())) {
            throw new RunnerException("Data is missing for ops " + auditLogMetaData.getOperation());
        }
    }

    public Map<String, AuditLogChanges> getChanges(BaseEntity newEntity, BaseEntity prevEntity, String operation) throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        List<Field> fields = null;
        if(newEntity == null && prevEntity == null)
            return new HashMap<>();
        if (operation.equals(DBOperationType.CREATE.name())) {
            fields = getListOfAllFields(newEntity);
        } else {
            fields = getListOfAllFields(prevEntity);
        }
        Map<String, AuditLogChanges> fieldValueMap = new HashMap<>();
        fields = fields.stream().filter(field -> !filterFieldBasedOnAnnotation(field)).toList();
        for (Field field : fields) {

            String fieldName = field.getName();

            if(fieldName.startsWith("cachedValue$") || fieldName.startsWith("$$_hibernate_interceptor" ) || fieldName.equalsIgnoreCase("serialVersionUID")) continue;
            Annotation[] fieldAnnotations = field.getDeclaredAnnotations();

            Object newValue, prevValue;
            AuditLogChanges auditLogChanges = null;


            if (operation.equals(DBOperationType.CREATE.name())) {
                Object temp = field.get(newEntity);
                try{
                    temp  = PropertyUtils.getProperty(newEntity, fieldName);
                } catch(NoSuchMethodException e){}
                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(temp)) {
                    newValue = temp.toString();
                } else {
                    newValue = temp;
                }
                if(Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotation.annotationType() == OneToOne.class))
                {
                    // fieldValueMap.putAll(getChanges((BaseEntity) temp, null, DBOperationType.CREATE.name()));
                    // Handle related entities (one-to-one or one-to-many relationships)
                    Map<String, AuditLogChanges> childChanges = getChanges((BaseEntity) temp, null, DBOperationType.CREATE.name());

                    // Prefix the property names with the entity name
                    for (Map.Entry<String, AuditLogChanges> entry : childChanges.entrySet()) {
                        String propertyName = fieldName + "." + entry.getKey();
                        fieldValueMap.put(propertyName, entry.getValue());
                    }
                }
                else if(newValue != null)
                    auditLogChanges = createAuditLogChangesObject(fieldName, newValue, null);

            }
            else if (operation.equals(DBOperationType.UPDATE.name())) {
                Object temp = field.get(newEntity);
                Object prevTemp = field.get(prevEntity);
                try{
                    temp  = PropertyUtils.getProperty(newEntity, fieldName);
                    prevTemp = PropertyUtils.getProperty(prevEntity, fieldName);
                } catch(NoSuchMethodException e){
                    log.error(e.getMessage());
                }

                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(temp)) {
                    newValue = temp.toString();
                } else {
                    newValue = temp;
                }

                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(prevTemp)) {
                    prevValue = prevTemp.toString();
                } else {
                    prevValue = prevTemp;
                }
                if (field.getType() == BigDecimal.class) {
                    BigDecimal number1 = (BigDecimal) newValue;
                    BigDecimal number2 = (BigDecimal) prevValue;
                    if((number1 == null) || (number1 != null && compareTo(number1, number2) != 0)) {
                        if (number1 != null && number2 != null && number1.setScale(5, BigDecimal.ROUND_DOWN).compareTo(number2.setScale(5, BigDecimal.ROUND_DOWN)) != 0) {
                            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
                        } else if (!(number1 == null && number2 == null)) {
                            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
                        }
                    }
                }
                else if(Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotation.annotationType() == OneToOne.class))
                {
                    var op = DBOperationType.UPDATE.name();
                    if(temp == null)
                        op = DBOperationType.DELETE.name();
                    else if(prevTemp == null)
                        op = DBOperationType.CREATE.name();
                    // Handle related entities (one-to-one or one-to-many relationships)
                    Map<String, AuditLogChanges> childChanges = getChanges((BaseEntity) temp, (BaseEntity) prevTemp, op);

                    // Prefix the property names with the entity name
                    for (Map.Entry<String, AuditLogChanges> entry : childChanges.entrySet()) {
                        String propertyName = fieldName + "." + entry.getKey();
                        fieldValueMap.put(propertyName, entry.getValue());
                    }
                }
                else if (!Objects.equals(newValue, prevValue)){
                    auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
                }

                else continue;

            }
            else if (operation.equals(DBOperationType.DELETE.name())) {
                Object prevTemp = field.get(prevEntity);
                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(prevTemp)) {
                    prevValue = prevTemp.toString();
                } else {
                    prevValue = prevTemp;
                }
                if(Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotation.annotationType() == OneToOne.class))
                {
                    Map<String, AuditLogChanges> childChanges = getChanges(null, (BaseEntity) prevTemp, DBOperationType.DELETE.name());

                    // Prefix the property names with the entity name
                    for (Map.Entry<String, AuditLogChanges> entry : childChanges.entrySet()) {
                        String propertyName = fieldName + "." + entry.getKey();
                        fieldValueMap.put(propertyName, entry.getValue());
                    }
                }
                else if(prevValue != null)
                    auditLogChanges = createAuditLogChangesObject(fieldName, null, prevValue);
            }
            if(auditLogChanges != null)
                fieldValueMap.put(fieldName, auditLogChanges);
        }

        return fieldValueMap;
    }

    public List<Field> getListOfAllFields(BaseEntity newEntity) {
        List<Field> privateFields = new ArrayList<>();
        Field[] allFields = newEntity.getClass().getDeclaredFields();
        for (Field field : allFields) {
            if (Modifier.isPrivate(field.getModifiers())) {
                privateFields.add(field);
                field.setAccessible(true);
            }
        }
        return privateFields;
    }

    public boolean filterFieldBasedOnAnnotation(Field field) {
        return Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotationClassList.contains(annotation.annotationType()));
    }

    public AuditLogChanges createAuditLogChangesObject(String fieldName, Object newValue, Object oldValue) {
        return AuditLogChanges.builder()
                .fieldName(fieldName)
                .newValue(newValue)
                .oldValue(oldValue)
                .build();
    }

    public static <T extends Comparable<T>> int compareTo(final T c1, final T c2) {
        final boolean f1, f2;
        return (f1 = c1 == null) ^ (f2 = c2 == null) ? f1 ? -1 : 1 : f1 && f2 ? 0 : c1.compareTo(c2);
    }
}
