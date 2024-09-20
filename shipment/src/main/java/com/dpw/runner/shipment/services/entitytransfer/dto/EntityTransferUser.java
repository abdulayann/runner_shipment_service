package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.LocalDateTime;
@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferUser implements IEntityTranferBaseEntity {
    private Long UserId;
    private String Username;
    private String Source;
    private String PasswordHash;
    private String PasswordSalt;
    private String DisplayName;
    private String Email;
    private String  UserImage;
    private String ImpersonateToken;
    private LocalDateTime LastDirectoryUpdate;
    private String CompanyCurrency;
    private String Password;
    private String OwnCaptcha;
    private String PasswordConfirm;
    private int Platform;
    private String EmployeeToken;
    private String ClearanceUrl;
}

