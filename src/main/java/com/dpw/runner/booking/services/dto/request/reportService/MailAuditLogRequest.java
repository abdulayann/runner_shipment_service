package com.dpw.runner.booking.services.dto.request.reportService;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MailAuditLogRequest {
    private List<Integer> tenantIds;
    private String body;
    private List<String> cc;
    private String emailAck;
    private String emailFrom;
    private String item;
    private String moduleName;
    private String scheduleName;
    private String sentBy;
    private LocalDateTime sentTime;
    private String subject;
    private List<String> to;
    private int userId;
}
