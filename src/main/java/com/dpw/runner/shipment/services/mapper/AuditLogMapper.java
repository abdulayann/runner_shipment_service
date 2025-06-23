package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.dto.response.AuditLogResponse;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.utils.Generated;
import org.mapstruct.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Mapper(
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
@Generated
public interface AuditLogMapper {

    @Mapping(source = "changes", target = "changes", qualifiedByName = "mapToListChanges")
    AuditLogResponse map(AuditLog req);

    @Named("mapToListChanges")
    default List<AuditLogChanges> mapToListChanges(Map<String, AuditLogChanges> req) {
        List<AuditLogChanges> changes = new ArrayList<>();
        for (Map.Entry<String, AuditLogChanges> entry : req.entrySet()) {
            AuditLogChanges auditChange = entry.getValue();
            auditChange.setFieldName(entry.getKey());
            changes.add(entry.getValue());
        }
        return changes;
    }
}
